{
     File:       QD/Fonts.h
 
     Contains:   Public interface to the Font Manager.
 
     Version:    Quickdraw-285~150
 
     Copyright:  © 1985-2008 by Apple Inc. all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Fonts;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,QuickdrawText,ATSTypes,Files,MacErrors,QuickdrawTypes,TextCommon,CGFont;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

{$ifc not TARGET_CPU_64}
{
 *  FMGetATSFontRefFromFont()
 *  
 *  Summary:
 *    Obtains the ATS font reference associated with a QuickDraw font
 *    reference.
 *  
 *  Parameters:
 *    
 *    iFont:
 *      A QuickDraw font reference.
 *    
 *    Result:
 *      The ATS font reference associated with the specified QuickDraw
 *      font reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetATSFontRefFromFont( iFont: FMFont ): ATSFontRef; external name '_FMGetATSFontRefFromFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  FMGetFontFromATSFontRef()
 *  
 *  Summary:
 *    Obtains the QuickDraw font reference associated with an ATS font
 *    reference.
 *  
 *  Parameters:
 *    
 *    iFont:
 *      An ATS font reference.
 *    
 *    Result:
 *      The QuickDraw font reference associated with the specified ATS
 *      font reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only]
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetFontFromATSFontRef( iFont: ATSFontRef ): FMFont; external name '_FMGetFontFromATSFontRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  FMFontGetCGFontRefFromFontFamilyInstance()
 *  
 *  Summary:
 *    Obtains the Quartz font reference associated with a typeface from
 *    a QuickDraw font family reference.
 *  
 *  Parameters:
 *    
 *    iFontFamily:
 *      A QuickDraw font family reference.
 *    
 *    iStyle:
 *      A QuickDraw font style.
 *    
 *    oFont:
 *      A pointer to a Quartz font reference. On output, points to the
 *      font reference for the specified font family and style. You are
 *      responsible for allocating the memory for the font reference.
 *    
 *    oStyle:
 *      On output, a pointer to an intrinsic font style. If a font
 *      reference isn’t found that matches the font family reference
 *      and font style you specify, the function returns the QuickDraw
 *      style that matches most closely.
 *    
 *    Result:
 *      A result code. If a font reference and intrinsic style are not
 *      found, the function returns a value of kFMInvalidFontErr.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in ApplicationServices.framework [32-bit only]
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FMFontGetCGFontRefFromFontFamilyInstance( iFontFamily: FMFontFamily; iStyle: FMFontStyle; var oFont: CGFontRef; var oStyle: FMFontStyle ): OSStatus; external name '_FMFontGetCGFontRefFromFontFamilyInstance';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  The remaining functions in this file have all been deprecated on Mac OS X 10.4. There are other
 *  solutions that are recommended that provide better compatibility with the rest of the operating
 *  system.
 *  
 *  Instead of using the QuickDraw functions, you should consider the following:
 *
 *  1.  For drawing and measuring text, you can use the Appearance Manager API in HITheme.h or the
 *      ATSUI API in ATSUnicode.h to render text directly through a Quartz graphics context. Alternatively
 *      use CoreText on Mac OS X 10.5 or later.
 *
 *  2.  For accessing information on fonts tracked by the operating system, please refer to the
 *      functions described in ATSFont.h. Alternatively use CoreText on Mac OS X 10.5 or later.
 *  
 *  3.  For accessing and modifying information on fonts in a Quartz graphics context, please refer
 *      to the functions described in CoreGraphics.h.
 }
{$endc} {not TARGET_CPU_64}

const
	systemFont = 0;
	applFont = 1;

const
	kFMDefaultOptions = kNilOptions;

{ Activation contexts }
const
	kFMDefaultActivationContext = kFMDefaultOptions;
	kFMGlobalActivationContext = $00000001;
	kFMLocalActivationContext = kFMDefaultActivationContext;

{ Iteration scopes }
const
	kFMDefaultIterationScope = kFMDefaultOptions;
	kFMGlobalIterationScope = $00000001;
	kFMLocalIterationScope = kFMDefaultIterationScope;

{ kPlatformDefaultGuiFontID is used in QuickTime 3.0. }
const
	kPlatformDefaultGuiFontID = applFont;

const
	commandMark = 17;
	checkMark = 18;
	diamondMark = 19;
	appleMark = 20;

const
	propFont = 36864;
	prpFntH = 36865;
	prpFntW = 36866;
	prpFntHW = 36867;
	fixedFont = 45056;
	fxdFntH = 45057;
	fxdFntW = 45058;
	fxdFntHW = 45059;
	fontWid = 44208;

type
	FontRec = record
		fontType: SInt16;               {font type}
		firstChar: SInt16;              {ASCII code of first character}
		lastChar: SInt16;               {ASCII code of last character}
		widMax: SInt16;                 {maximum character width}
		kernMax: SInt16;                {negative of maximum character kern}
		nDescent: SInt16;               {negative of descent}
		fRectWidth: SInt16;             {width of font rectangle}
		fRectHeight: SInt16;            {height of font rectangle}
		owTLoc: UInt16;                 {offset to offset/width table}
		ascent: SInt16;                 {ascent}
		descent: SInt16;                {descent}
		leading: SInt16;                {leading}
		rowWords: SInt16;               {row width of bit image / 2 }
	end;
	FontRecPtr = ^FontRec;
type
	FontRecHdl = ^FontRecPtr;
	FMInput = record
		family: SInt16;
		size: SInt16;
		face: Style;
		needBits: Boolean;
		device: SInt16;
		numer: Point;
		denom: Point;
	end;
	FMInputPtr = ^FMInput;
type
	FamRecPtr = ^FamRec;
	FamRec = record
		ffFlags: SInt16;                {flags for family}
		ffFamID: SInt16;                {family ID number}
		ffFirstChar: SInt16;            {ASCII code of 1st character}
		ffLastChar: SInt16;             {ASCII code of last character}
		ffAscent: SInt16;               {maximum ascent for 1pt font}
		ffDescent: SInt16;              {maximum descent for 1pt font}
		ffLeading: SInt16;              {maximum leading for 1pt font}
		ffWidMax: SInt16;               {maximum widMax for 1pt font}
		ffWTabOff: SInt32;              {offset to width table}
		ffKernOff: SInt32;              {offset to kerning table}
		ffStylOff: SInt32;              {offset to style mapping table}
		ffProperty: array [0..8] of SInt16;          {style property info}
		ffIntl: array [0..1] of SInt16;              {for international use}
		ffVersion: SInt16;              {version number}
	end;
type
	AsscEntryPtr = ^AsscEntry;
	AsscEntry = record
		fontSize: SInt16;
		fontStyle: SInt16;
		fontID: SInt16;                 {font resource ID}
	end;
type
	FontAssocPtr = ^FontAssoc;
	FontAssoc = record
		numAssoc: SInt16;               {number of entries - 1}
	end;
type
	StyleTablePtr = ^StyleTable;
	StyleTable = record
		fontClass: SInt16;
		offset: SInt32;
		reserved: SInt32;
		indexes: array [0..47] of SInt8;
	end;
type
	NameTablePtr = ^NameTable;
	NameTable = record
		stringCount: SInt16;
		baseFontName: Str255;
	end;
type
	KernPairPtr = ^KernPair;
	KernPair = record
		kernFirst: char;              {1st character of kerned pair}
		kernSecond: char;             {2nd character of kerned pair}
		kernWidth: SInt16;              {kerning in 1pt fixed format}
	end;
type
	KernEntryPtr = ^KernEntry;
	KernEntry = record
		kernStyle: SInt16;              {style the entry applies to}
		kernLength: SInt16;             {length of this entry}
	end;
type
	KernTablePtr = ^KernTable;
	KernTable = record
		numKerns: SInt16;               {number of kerning entries}
	end;
	FMOutput = record
		errNum: SInt16;
		fontHandle: Handle;
		boldPixels: UInt8;
		italicPixels: UInt8;
		ulOffset: UInt8;
		ulShadow: UInt8;
		ulThick: UInt8;
		shadowPixels: UInt8;
		extra: SInt8;
		ascent: UInt8;
		descent: UInt8;
		widMax: UInt8;
		leading: SInt8;
		curStyle: SInt8;
		numer: Point;
		denom: Point;
	end;
	FMOutputPtr = ^FMOutput;
type
	FMOutPtr = FMOutputPtr;
	FMetricRec = record
		ascent: Fixed;                 {base line to top}
		descent: Fixed;                {base line to bottom}
		leading: Fixed;                {leading between lines}
		widMax: Fixed;                 {maximum character width}
		wTabHandle: Handle;             {handle to font width table}
	end;
	FMetricRecPtr = ^FMetricRec;
type
	FMetricRecHandle = ^FMetricRecPtr;
{$ifc not TARGET_CPU_64}
{
 *  InitFonts()
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only]
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  GetFontName()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure GetFontName( familyID: SInt16; var name: Str255 ); external name '_GetFontName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  GetFNum()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure GetFNum( const (*var*) name: Str255; var familyID: SInt16 ); external name '_GetFNum';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  RealFont()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function RealFont( fontNum: SInt16; size: SInt16 ): Boolean; external name '_RealFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetFontLock()
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only]
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  FMSwapFont()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FMSwapFont( const (*var*) inRec: FMInput ): FMOutPtr; external name '_FMSwapFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetFScaleDisable()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetFScaleDisable( fscaleDisable: Boolean ); external name '_SetFScaleDisable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FontMetrics()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure FontMetrics( theMetrics: FMetricRecPtr ); external name '_FontMetrics';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetFractEnable()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetFractEnable( fractEnable: Boolean ); external name '_SetFractEnable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  GetDefFontSize()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetDefFontSize: SInt16; external name '_GetDefFontSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  IsOutline()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IsOutline( numer: Point; denom: Point ): Boolean; external name '_IsOutline';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetOutlinePreferred()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetOutlinePreferred( outlinePreferred: Boolean ); external name '_SetOutlinePreferred';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  GetOutlinePreferred()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetOutlinePreferred: Boolean; external name '_GetOutlinePreferred';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  OutlineMetrics()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function OutlineMetrics( byteCount: SInt16; textPtr: {const} UnivPtr; numer: Point; denom: Point; var yMax: SInt16; var yMin: SInt16; awArray: FixedPtr; lsbArray: FixedPtr; boundsArray: RectPtr ): OSErr; external name '_OutlineMetrics';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  SetPreserveGlyph()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SetPreserveGlyph( preserveGlyph: Boolean ); external name '_SetPreserveGlyph';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  GetPreserveGlyph()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetPreserveGlyph: Boolean; external name '_GetPreserveGlyph';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{$endc} {not TARGET_CPU_64}

{
 *  FlushFonts()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{$ifc not TARGET_CPU_64}
{
 *  getfnum()
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only]
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  getfontname()
 *  
 *  Availability:
 *    Mac OS X:         not available [32-bit only]
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  GetSysFont()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetSysFont: SInt16; external name '_GetSysFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  GetAppFont()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetAppFont: SInt16; external name '_GetAppFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ Extended font data functions (available only with Mac OS 8.5 or later). }
{
 *  SetAntiAliasedTextEnabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 }
function SetAntiAliasedTextEnabled( iEnable: Boolean; iMinFontSize: SInt16 ): OSStatus; external name '_SetAntiAliasedTextEnabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  IsAntiAliasedTextEnabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 }
function IsAntiAliasedTextEnabled( var oMinFontSize: SInt16 ): Boolean; external name '_IsAntiAliasedTextEnabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  QDTextBounds()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 }
procedure QDTextBounds( byteCount: SInt16; textAddr: {const} UnivPtr; var bounds: Rect ); external name '_QDTextBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FetchFontInfo()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.6 and later
 }
function FetchFontInfo( fontID: SInt16; fontSize: SInt16; fontStyle: SInt16; var info: FontInfo ): OSErr; external name '_FetchFontInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ Font access and data management functions (available only with Mac OS 9.0 or later). }
{
 *  FMCreateFontFamilyIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyIteratorCreate.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMCreateFontFamilyIterator( {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontFamilyIterator ): OSStatus; external name '_FMCreateFontFamilyIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMDisposeFontFamilyIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyIteratorRelease.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMDisposeFontFamilyIterator( var ioIterator: FMFontFamilyIterator ): OSStatus; external name '_FMDisposeFontFamilyIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMResetFontFamilyIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyIteratorReset.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMResetFontFamilyIterator( {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontFamilyIterator ): OSStatus; external name '_FMResetFontFamilyIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetNextFontFamily()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyIteratorNext.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetNextFontFamily( var ioIterator: FMFontFamilyIterator; var oFontFamily: FMFontFamily ): OSStatus; external name '_FMGetNextFontFamily';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMCreateFontIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontIteratorCreate.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMCreateFontIterator( {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontIterator ): OSStatus; external name '_FMCreateFontIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMDisposeFontIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontIteratorRelease.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMDisposeFontIterator( var ioIterator: FMFontIterator ): OSStatus; external name '_FMDisposeFontIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMResetFontIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontIteratorReset.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMResetFontIterator( {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits; var ioIterator: FMFontIterator ): OSStatus; external name '_FMResetFontIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetNextFont()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontIteratorNext.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetNextFont( var ioIterator: FMFontIterator; var oFont: FMFont ): OSStatus; external name '_FMGetNextFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMCreateFontFamilyInstanceIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMCreateFontFamilyInstanceIterator( iFontFamily: FMFontFamily; var ioIterator: FMFontFamilyInstanceIterator ): OSStatus; external name '_FMCreateFontFamilyInstanceIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMDisposeFontFamilyInstanceIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMDisposeFontFamilyInstanceIterator( var ioIterator: FMFontFamilyInstanceIterator ): OSStatus; external name '_FMDisposeFontFamilyInstanceIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMResetFontFamilyInstanceIterator()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMResetFontFamilyInstanceIterator( iFontFamily: FMFontFamily; var ioIterator: FMFontFamilyInstanceIterator ): OSStatus; external name '_FMResetFontFamilyInstanceIterator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetNextFontFamilyInstance()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetNextFontFamilyInstance( var ioIterator: FMFontFamilyInstanceIterator; var oFont: FMFont; oStyle: FMFontStylePtr { can be NULL }; oSize: FMFontSizePtr { can be NULL } ): OSStatus; external name '_FMGetNextFontFamilyInstance';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyFromName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyFindFromName.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFamilyFromName( const (*var*) iName: Str255 ): FMFontFamily; external name '_FMGetFontFamilyFromName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyGetName.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFamilyName( iFontFamily: FMFontFamily; var oName: Str255 ): OSStatus; external name '_FMGetFontFamilyName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyTextEncoding()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyGetEncoding.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFamilyTextEncoding( iFontFamily: FMFontFamily; var oTextEncoding: TextEncoding ): OSStatus; external name '_FMGetFontFamilyTextEncoding';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyGeneration()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontFamilyGetGeneration.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFamilyGeneration( iFontFamily: FMFontFamily; var oGeneration: FMGeneration ): OSStatus; external name '_FMGetFontFamilyGeneration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFormat()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFormat( iFont: FMFont; var oFormat: FourCharCode ): OSStatus; external name '_FMGetFontFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontTableDirectory()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontTableDirectory( iFont: FMFont; iLength: ByteCount; iBuffer: UnivPtr; oActualLength: ByteCountPtr { can be NULL } ): OSStatus; external name '_FMGetFontTableDirectory';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontTable()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontGetTable or CTFontCopyTable from
 *    CoreText/CTFont.h.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontTable( iFont: FMFont; iTag: FourCharCode; iOffset: ByteOffset; iLength: ByteCount; iBuffer: UnivPtr; oActualLength: ByteCountPtr { can be NULL } ): OSStatus; external name '_FMGetFontTable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontGeneration()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontGetGeneration.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontGeneration( iFont: FMFont; var oGeneration: FMGeneration ): OSStatus; external name '_FMGetFontGeneration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontContainer()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontGetContainer.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontContainer( iFont: FMFont; var oFontContainer: FSSpec ): OSStatus; external name '_FMGetFontContainer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFromFontFamilyInstance()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is CTFontCreateFromQuickdrawInstance in
 *    CoreText/CTFont.h.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFromFontFamilyInstance( iFontFamily: FMFontFamily; iStyle: FMFontStyle; var oFont: FMFont; oIntrinsicStyle: FMFontStylePtr { can be NULL } ): OSStatus; external name '_FMGetFontFromFontFamilyInstance';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyInstanceFromFont()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetFontFamilyInstanceFromFont( iFont: FMFont; var oFontFamily: FMFontFamily; var oStyle: FMFontStyle ): OSStatus; external name '_FMGetFontFamilyInstanceFromFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetATSFontFamilyRefFromFontFamily()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetATSFontFamilyRefFromFontFamily( iFamily: FMFontFamily ): ATSFontFamilyRef; external name '_FMGetATSFontFamilyRefFromFontFamily';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyFromATSFontFamilyRef()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetFontFamilyFromATSFontFamilyRef( iFamily: ATSFontFamilyRef ): FMFontFamily; external name '_FMGetFontFamilyFromATSFontFamilyRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMActivateFonts()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontActivateFromFileReference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMActivateFonts( const (*var*) iFontContainer: FSSpec; {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits ): OSStatus; external name '_FMActivateFonts';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMDeactivateFonts()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontDeactivate.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMDeactivateFonts( const (*var*) iFontContainer: FSSpec; {const} iFilter: FMFilterPtr { can be NULL }; iRefCon: UnivPtr; iOptions: OptionBits ): OSStatus; external name '_FMDeactivateFonts';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ Use ATSGetGeneration instead of FMGetGeneration }
{
 *  FMGetGeneration()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSGetGeneration.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in FontManager 9.0 and later
 }
function FMGetGeneration: FMGeneration; external name '_FMGetGeneration';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  FMGetFontContainerFromFontFamilyInstance()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontGetContainer.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetFontContainerFromFontFamilyInstance( iFontFamily: FMFontFamily; iStyle: FMFontStyle; iFontSize: FMFontSize; var oFontContainer: FSSpec ): OSStatus; external name '_FMGetFontContainerFromFontFamilyInstance';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{
 *  FMGetFontFamilyResource()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated in Mac OS X 10.4 and not available in
 *    64-bit. Please see ATSFont.h for alternatives. Suggested
 *    replacement is ATSFontGetFontFamilyResource.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FMGetFontFamilyResource( iFontFamily: FMFontFamily; iFontStyle: FMFontStyle; iFontSize: FMFontSize; iBufferSize: ByteCount; ioBuffer: UnivPtr; oSize: ByteCountPtr { can be NULL } ): OSStatus; external name '_FMGetFontFamilyResource';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{$endc} {not TARGET_CPU_64}

type
	FontFamilyID = FMFontFamily;
	FontPointSize = FMFontSize;
const
	kFMUseGlobalScopeOption = $00000001;

{
 *  The font identifier constants are deprecated; use ATSFontFamilyFindFromQuickDrawName in ATSFont.h
 *  to find a font family from a standard QuickDraw name.
 }
const
	kFontIDNewYork = 2;
	kFontIDGeneva = 3;
	kFontIDMonaco = 4;
	kFontIDVenice = 5;
	kFontIDLondon = 6;
	kFontIDAthens = 7;
	kFontIDSanFrancisco = 8;
	kFontIDToronto = 9;
	kFontIDCairo = 11;
	kFontIDLosAngeles = 12;
	kFontIDTimes = 20;
	kFontIDHelvetica = 21;
	kFontIDCourier = 22;
	kFontIDSymbol = 23;
	kFontIDMobile = 24;

{
 *  The following data structures referenced by the low memory global variables of QuickDraw are
 *  deprecated on Mac OS X and CarbonLib 1.1. The low memory global variables are not shared between
 *  processes and may result in inconsistencies compared to previous releases of the system software.
 *  Changes made to the information contained in the low memory global variables, including any
 *  indirectly referenced width tables, font family records, and font records, are not reflected in
 *  the global state for QuickDraw and should only be accessed through the font access and data
 *  management functions of ATS.
 }
type
	WidEntryPtr = ^WidEntry;
	WidEntry = record
		widStyle: SInt16;               {style entry applies to}
	end;
type
	WidTablePtr = ^WidTable;
	WidTable = record
		numWidths: SInt16;              {number of entries - 1}
	end;
type
	WidthTable = packed record
		tabData: array [0..255] of Fixed;           {character widths}
		tabFont: Handle;                {font record used to build table}
		sExtra: SIGNEDLONG;                 {space extra used for table}
		style: SIGNEDLONG;                  {extra due to style}
		fID: SInt16;                    {font family ID}
		fSize: SInt16;                  {font size request}
		face: SInt16;                   {style (face) request}
		device: SInt16;                 {device requested}
		inNumer: Point;                {scale factors requested}
		inDenom: Point;                {scale factors requested}
		aFID: SInt16;                   {actual font family ID for table}
		fHand: Handle;                  {family record used to build up table}
		usedFam: Boolean;                {used fixed point family widths}
		aFace: UInt8;                  {actual face produced}
		vOutput: SInt16;                {vertical scale output value}
		hOutput: SInt16;                {horizontal scale output value}
		vFactor: SInt16;                {vertical scale output value}
		hFactor: SInt16;                {horizontal scale output value}
		aSize: SInt16;                  {actual size of actual font used}
		tabSize: SInt16;                {total size of table}
	end;
	WidthTablePtr = ^WidthTable;
type
	WidthTableHdl = ^WidthTablePtr;
{$ifc OLDROUTINENAMES}
const
	newYork = kFontIDNewYork;
	geneva = kFontIDGeneva;
	monaco = kFontIDMonaco;
	venice = kFontIDVenice;
	london = kFontIDLondon;
	athens = kFontIDAthens;
	sanFran = kFontIDSanFrancisco;
	toronto = kFontIDToronto;
	cairo = kFontIDCairo;
	losAngeles = kFontIDLosAngeles;
	times = kFontIDTimes;
	helvetica = kFontIDHelvetica;
	courier = kFontIDCourier;
	symbol = kFontIDSymbol;
	mobile = kFontIDMobile;

{$endc} {OLDROUTINENAMES}


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

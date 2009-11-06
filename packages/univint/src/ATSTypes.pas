{
     File:       ATS/ATSTypes.h
 
     Contains:   Public interfaces for Apple Type Services components.
 
     Copyright:  © 1997-2008 by Apple Inc., all rights reserved.
 
     Warning:    *** APPLE INTERNAL USE ONLY ***
                 This file may contain unreleased API's
 
     BuildInfo:  Built by:            root
                 On:                  Fri Jul 24 22:21:51 2009
                 With Interfacer:     3.0d46   (Mac OS X for PowerPC)
                 From:                ATSTypes.i
                     Revision:        1.14
                     Dated:           2007/04/24 03:44:18
                     Last change by:  juliog
                     Last comment:    <rdar://problem/5152407> File based filter for 64bit causes binary
 
     Bugs:       Report bugs to Radar component "System Interfaces", "Latest"
                 List the version information (from above) in the Problem Description.
 
}

{ Pascal Translation Updated: Gorazd Krosl <gorazd_1957@yahoo.ca>, October 2009 }

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

unit ATSTypes;
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
uses MacTypes,Files,CGBase,CGGeometry;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


//#pragma pack(push, 2)


{$ifc TARGET_CPU_64}
type
	ATSPoint = CGPoint;
{$elsec}
type
	ATSPoint = Float32Point;
{$endc}  { TARGET_CPU_64 }

{$ifc TARGET_CPU_64}
{
   ATSFSSpec serves as a temporary place holder for the FSSpec data type which is deprecated for 64-bit. 
   A 64-bit replacement for FSSpec based APIs will be introduced.  
}
type
	ATSFSSpec = record
		vRefNum: FSVolumeRefNum;
		parID: SInt32;
		name: StrFileName;
	end;
{$elsec}
type
	ATSFSSpec = FSSpec;
{$endc}  { TARGET_CPU_64 }
	
	ATSFSSpecPtr = ^ATSFSSpec;
	
{ FMGeneration }

	FMGeneration = UInt32;
{ The FMFontFamily reference represents a collection of fonts with the same design
   characteristics. It replaces the standard QuickDraw font identifer and may be used
   with all QuickDraw functions including GetFontName and TextFont. It cannot be used
   with the Resource Manager to access information from a FOND resource handle. A font
   reference does not imply a particular script system, nor is the character encoding
   of a font family determined by an arithmetic mapping of its value.
}

	FMFontFamily = SInt16;
	FMFontFamilyPtr = ^FMFontFamily;
	
	FMFontStyle = SInt16;
	FMFontStylePtr = ^FMFontStyle;
	
	FMFontSize = SInt16;
	FMFontSizePtr = ^FMFontSize;
	
{ 
   The font family is a collection of fonts, each of which is identified
   by an FMFont reference that maps to a single object registered with
   the font database. The font references associated with the font
   family consist of individual outline and bitmapped fonts that may be
   used with the font access routines of the Font Manager and ATS.
}

	FMFont = UInt32;
	FMFontPtr = ^FMFont;
	
	FMFontFamilyInstancePtr = ^FMFontFamilyInstance;
	FMFontFamilyInstance = record
		fontFamily: FMFontFamily;
		fontStyle: FMFontStyle;
	end;

	FMFontFamilyIterator = record
		reserved: array[0..15] of UInt32;
	end;
	FMFontFamilyIteratorPtr = ^FMFontFamilyIterator;


	FMFontIterator = record
		reserved: array[0..15] of UInt32;
	end;
	FMFontIteratorPtr = ^FMFontIterator;
	

	FMFontFamilyInstanceIterator = record
		reserved: array[0..15] of UInt32;
	end;
	FMFontFamilyInstanceIteratorPtr = ^FMFontFamilyInstanceIterator;
	
const
	kInvalidGeneration = 0;
	kInvalidFontFamily = -1;
	kInvalidFont = 0;


	kFMCurrentFilterFormat = 0;

type
	FMFilterSelector = UInt32;
const
	kFMFontTechnologyFilterSelector = 1;
	kFMFontContainerFilterSelector = 2;
	kFMGenerationFilterSelector = 3;
	kFMFontFamilyCallbackFilterSelector = 4;
	kFMFontCallbackFilterSelector = 5;
	kFMFontDirectoryFilterSelector = 6;
	kFMFontFileRefFilterSelector = 10;


	kFMTrueTypeFontTechnology = FourCharCode('true');
	kFMPostScriptFontTechnology = FourCharCode('typ1');

type
	FMFontFamilyCallbackFilterProcPtr = function( iFontFamily: FMFontFamily; iRefCon: UnivPtr ): OSStatus;
	FMFontCallbackFilterProcPtr = function( iFont: FMFont; iRefCon: UnivPtr ): OSStatus;
	FMFontFamilyCallbackFilterUPP = FMFontFamilyCallbackFilterProcPtr;
	FMFontCallbackFilterUPP = FMFontCallbackFilterProcPtr;

{
 *  NewFMFontFamilyCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewFMFontFamilyCallbackFilterUPP( userRoutine: FMFontFamilyCallbackFilterProcPtr ): FMFontFamilyCallbackFilterUPP; external name '_NewFMFontFamilyCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewFMFontCallbackFilterUPP( userRoutine: FMFontCallbackFilterProcPtr ): FMFontCallbackFilterUPP; external name '_NewFMFontCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeFMFontFamilyCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeFMFontFamilyCallbackFilterUPP( userUPP: FMFontFamilyCallbackFilterUPP ); external name '_DisposeFMFontFamilyCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeFMFontCallbackFilterUPP( userUPP: FMFontCallbackFilterUPP ); external name '_DisposeFMFontCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeFMFontFamilyCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeFMFontFamilyCallbackFilterUPP( iFontFamily: FMFontFamily; iRefCon: UnivPtr; userUPP: FMFontFamilyCallbackFilterUPP ): OSStatus; external name '_InvokeFMFontFamilyCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeFMFontCallbackFilterUPP( iFont: FMFont; iRefCon: UnivPtr; userUPP: FMFontCallbackFilterUPP ): OSStatus; external name '_InvokeFMFontCallbackFilterUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)
{
#if __MACH__
    #define NewFMFontFamilyCallbackFilterUPP(userRoutine)       ((FMFontFamilyCallbackFilterUPP)userRoutine)
    #define NewFMFontCallbackFilterUPP(userRoutine)             ((FMFontCallbackFilterUPP)userRoutine)
    #define DisposeFMFontFamilyCallbackFilterUPP(userUPP)
    #define DisposeFMFontCallbackFilterUPP(userUPP)
    #define InvokeFMFontFamilyCallbackFilterUPP(iFontFamily, iRefCon, userUPP) (*userUPP)(iFontFamily, iRefCon)
    #define InvokeFMFontCallbackFilterUPP(iFont, iRefCon, userUPP) (*userUPP)(iFont, iRefCon)
#endif
}
type
	FMFontDirectoryFilter = record
		fontFolderDomain: SInt16;
		reserved: array[0..1] of UInt32;
	end;
	FMFontDirectoryFilterPtr = ^FMFontDirectoryFilter;
	
{
   Note: The fontContainerFilter member is not available in 64-bit. Use fontFileRefFilter
   and the kFMFontFileRefFilterSelector enum instead.
}
type
	FMFilterfilter = record
		case Byte of
			0:	(fontTechnologyFilter: FourCharCode);
			1:	(fontContainerFilter: ATSFSSpec);
			2:	(generationFilter: FMGeneration);
			3:	(fontFamilyCallbackFilter: FMFontFamilyCallbackFilterUPP);
			4:	(fontCallbackFilter: FMFontCallbackFilterUPP);
			5:	(fontDirectoryFilter: FMFontDirectoryFilter);
			6:	(fontFileRefFilter: { const } FSRefPtr);
	end;
	
	FMFilter = record
		format: UInt32;
		selector: FMFilterSelector;
		filter : FMFilterfilter;
	end;
	FMFilterPtr = ^FMFilter;
	
	ATSOptionFlags = OptionBits;
	ATSGeneration = UInt32;
	ATSFontContainerRef = UInt32;
	
	ATSFontFamilyRef = UInt32;
	ATSFontFamilyRefPtr = ^ATSFontFamilyRef;
	
	ATSFontRef = UInt32;
	ATSFontRefPtr = ^ATSFontRef;

	ATSGlyphRef = UInt16;
	ATSGlyphRefPtr = ^ATSGlyphRef;
	
	ATSFontSize = CGFloat;
	ATSFontFormat = UInt32;
const
	kATSFontFormatUnspecified = 0;


	kATSGenerationUnspecified = 0;
	kATSFontContainerRefUnspecified = 0;
	kATSFontFamilyRefUnspecified = 0;
	kATSFontRefUnspecified = 0;

{
    ATSFontMetrics measurements are relative to a font's point size.
    For example, when a font with an ATSFontMetrics ascent of 0.6 is drawn at 18 points, its actual ascent is (0.6 * 18) = 10.8 points.
}
type
	ATSFontMetrics = record
		version: UInt32;
		ascent: CGFloat;                 { Maximum height above baseline reached by the glyphs in the font }
                                              { or maximum distance to the right of the centerline reached by the glyphs in the font }
		descent: CGFloat;                { Maximum depth below baseline reached by the glyphs in the font }
                                              { or maximum distance to the left of the centerline reached by the glyphs in the font }
		leading: CGFloat;                { Desired spacing between lines of text }
		avgAdvanceWidth: CGFloat;
		maxAdvanceWidth: CGFloat;        { Maximum advance width or height of the glyphs in the font }
		minLeftSideBearing: CGFloat;     { Minimum left or top side bearing }
		minRightSideBearing: CGFloat;    { Minimum right or bottom side bearing }
		stemWidth: CGFloat;              { Width of the dominant vertical stems of the glyphs in the font }
		stemHeight: CGFloat;             { Vertical width of the dominant horizontal stems of glyphs in the font }
		capHeight: CGFloat;              { Height of a capital letter from the baseline to the top of the letter }
		xHeight: CGFloat;                { Height of lowercase characters in a font, specifically the letter x, excluding ascenders and descenders }
		italicAngle: CGFloat;            { Angle in degrees counterclockwise from the vertical of the dominant vertical strokes of the glyphs in the font }
		underlinePosition: CGFloat;      { Distance from the baseline for positioning underlining strokes }
		underlineThickness: CGFloat;     { Stroke width for underlining }
	end;
	ATSFontMetricsPtr = ^ATSFontMetrics;
	
const
	kATSItalicQDSkew = (1 shl 16) / 4; { fixed value of 0.25 }
	kATSBoldQDStretch = (1 shl 16) * 3 / 2; { fixed value of 1.50 }
	kATSRadiansFactor = 1144;  { fixed value of approx. pi/180 (0.0174560546875) }

{ Glyph outline path constants used in ATSFontGetNativeCurveType. }
type
	ATSCurveType = UInt16;
const
	kATSCubicCurveType = $0001;
	kATSQuadCurveType = $0002;
	kATSOtherCurveType = $0003;

{ 
    This is what the ATSGlyphRef is set to when the glyph is deleted -
    that is, when the glyph is set to no longer appear when the layout
    is actually drawn
}

	kATSDeletedGlyphcode = $FFFF;

type
	ATSUCurvePathPtr = ^ATSUCurvePath;
	ATSUCurvePath = record
		vectors: UInt32;
  		controlBits: array[0..0] of UInt32;
        vector: array[0..0] of ATSPoint;
	end;

	ATSUCurvePathsPtr = ^ATSUCurvePaths;
	ATSUCurvePaths = record
		contours: UInt32;
        contour: array[0..0] of ATSUCurvePath;
	end;
{ Glyph ideal metrics }

	ATSGlyphIdealMetricsPtr = ^ATSGlyphIdealMetrics;
	ATSGlyphIdealMetrics = record
		advance: ATSPoint;
		sideBearing: ATSPoint;
		otherSideBearing: ATSPoint;
	end;
{ Glyph screen metrics }

	ATSGlyphScreenMetricsPtr = ^ATSGlyphScreenMetrics;
	ATSGlyphScreenMetrics = record
		deviceAdvance: ATSPoint;
		topLeft: ATSPoint;
		height: UInt32;
		width: UInt32;
		sideBearing: ATSPoint;
		otherSideBearing: ATSPoint;
	end;
{ Glyph References }

	GlyphID = ATSGlyphRef;
	GlyphIDPtr = ^GlyphID;
	GlyphID_fix = GlyphID;


{$endc} {TARGET_OS_MAC}

	{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

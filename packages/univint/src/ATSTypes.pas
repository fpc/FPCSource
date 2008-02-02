{
     File:       ATSTypes.p
 
     Contains:   Public interfaces for Apple Type Services components.
 
     Version:    Technology: Mac OS 9 / Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1997-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit ATSTypes;
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
uses MacTypes,Files,MixedMode;


{$ALIGN MAC68K}


type
	FMGeneration						= UInt32;
	{	 The FMFontFamily reference represents a collection of fonts with the same design
	   characteristics. It replaces the standard QuickDraw font identifer and may be used
	   with all QuickDraw functions including GetFontName and TextFont. It cannot be used
	   with the Resource Manager to access information from a FOND resource handle. A font
	   reference does not imply a particular script system, nor is the character encoding
	   of a font family determined by an arithmetic mapping of its value.
		}
	FMFontFamily						= SInt16;
	FMFontStyle							= SInt16;
	FMFontStylePtr						= ^FMFontStyle; { when a VAR xx: FMFontStyle parameter can be nil, it is changed to xx: FMFontStylePtr }
	FMFontSize							= SInt16;
	FMFontSizePtr						= ^FMFontSize; { when a VAR xx: FMFontSize parameter can be nil, it is changed to xx: FMFontSizePtr }
	{	 
	   The font family is a collection of fonts, each of which is identified
	   by an FMFont reference that maps to a single object registered with
	   the font database. The font references associated with the font
	   family consist of individual outline and bitmapped fonts that may be
	   used with the font access routines of the Font Manager and ATS.
		}
	FMFont								= UInt32;
	FMFontPtr							= ^FMFont; { when a VAR xx: FMFont parameter can be nil, it is changed to xx: FMFontPtr }
	FMFontFamilyInstancePtr = ^FMFontFamilyInstance;
	FMFontFamilyInstance = record
		fontFamily:				FMFontFamily;
		fontStyle:				FMFontStyle;
	end;

	FMFontFamilyIteratorPtr = ^FMFontFamilyIterator;
	FMFontFamilyIterator = record
		reserved:				array [0..15] of UInt32;
	end;

	FMFontIteratorPtr = ^FMFontIterator;
	FMFontIterator = record
		reserved:				array [0..15] of UInt32;
	end;

	FMFontFamilyInstanceIteratorPtr = ^FMFontFamilyInstanceIterator;
	FMFontFamilyInstanceIterator = record
		reserved:				array [0..15] of UInt32;
	end;


const
	kInvalidGeneration			= 0;
	kInvalidFontFamily			= -1;
	kInvalidFont				= 0;

	kFMCurrentFilterFormat		= 0;

	{	 kFMDefaultOptions & kFMUseGlobalScopeOption moved to Fonts.h 	}

type
	FMFilterSelector 			= UInt32;
const
	kFMFontTechnologyFilterSelector = 1;
	kFMFontContainerFilterSelector = 2;
	kFMGenerationFilterSelector	= 3;
	kFMFontFamilyCallbackFilterSelector = 4;
	kFMFontCallbackFilterSelector = 5;
	kFMFontDirectoryFilterSelector = 6;

	kFMTrueTypeFontTechnology	= $74727565 (* 'true' *);
	kFMPostScriptFontTechnology	= $74797031 (* 'typ1' *);


type
{$ifc TYPED_FUNCTION_POINTERS}
	FMFontFamilyCallbackFilterProcPtr = function(iFontFamily: FMFontFamily; iRefCon: UnivPtr): OSStatus;
{$elsec}
	FMFontFamilyCallbackFilterProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	FMFontCallbackFilterProcPtr = function(iFont: FMFont; iRefCon: UnivPtr): OSStatus;
{$elsec}
	FMFontCallbackFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	FMFontFamilyCallbackFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FMFontFamilyCallbackFilterUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	FMFontCallbackFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FMFontCallbackFilterUPP = UniversalProcPtr;
{$endc}	

const
	uppFMFontFamilyCallbackFilterProcInfo = $000003B0;
	uppFMFontCallbackFilterProcInfo = $000003F0;
	{
	 *  NewFMFontFamilyCallbackFilterUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewFMFontFamilyCallbackFilterUPP(userRoutine: FMFontFamilyCallbackFilterProcPtr): FMFontFamilyCallbackFilterUPP; external name '_NewFMFontFamilyCallbackFilterUPP'; { old name was NewFMFontFamilyCallbackFilterProc }
{
 *  NewFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewFMFontCallbackFilterUPP(userRoutine: FMFontCallbackFilterProcPtr): FMFontCallbackFilterUPP; external name '_NewFMFontCallbackFilterUPP'; { old name was NewFMFontCallbackFilterProc }
{
 *  DisposeFMFontFamilyCallbackFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeFMFontFamilyCallbackFilterUPP(userUPP: FMFontFamilyCallbackFilterUPP); external name '_DisposeFMFontFamilyCallbackFilterUPP';
{
 *  DisposeFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeFMFontCallbackFilterUPP(userUPP: FMFontCallbackFilterUPP); external name '_DisposeFMFontCallbackFilterUPP';
{
 *  InvokeFMFontFamilyCallbackFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeFMFontFamilyCallbackFilterUPP(iFontFamily: FMFontFamily; iRefCon: UnivPtr; userRoutine: FMFontFamilyCallbackFilterUPP): OSStatus; external name '_InvokeFMFontFamilyCallbackFilterUPP'; { old name was CallFMFontFamilyCallbackFilterProc }
{
 *  InvokeFMFontCallbackFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeFMFontCallbackFilterUPP(iFont: FMFont; iRefCon: UnivPtr; userRoutine: FMFontCallbackFilterUPP): OSStatus; external name '_InvokeFMFontCallbackFilterUPP'; { old name was CallFMFontCallbackFilterProc }
type
	FMFontDirectoryFilterPtr = ^FMFontDirectoryFilter;
	FMFontDirectoryFilter = record
		fontFolderDomain:		SInt16;
		reserved:				array [0..1] of UInt32;
	end;

	FMFilterPtr = ^FMFilter;
	FMFilter = record
		format:					UInt32;
		selector:				FMFilterSelector;
		case SInt16 of
		0: (
			fontTechnologyFilter: FourCharCode;
			);
		1: (
			fontContainerFilter: FSSpec;
			);
		2: (
			generationFilter:	FMGeneration;
			);
		3: (
			fontFamilyCallbackFilter: FMFontFamilyCallbackFilterUPP;
			);
		4: (
			fontCallbackFilter:	FMFontCallbackFilterUPP;
			);
		5: (
			fontDirectoryFilter: FMFontDirectoryFilter;
			);
	end;

	ATSOptionFlags						= OptionBits;
	ATSGeneration						= UInt32;
	ATSFontContainerRef					= UInt32;
	ATSFontFamilyRef					= UInt32;
	ATSFontRef							= UInt32;
	ATSGlyphRef							= UInt16;
	ATSFontSize							= Float32;

const
	kATSGenerationUnspecified	= 0;
	kATSFontContainerRefUnspecified = 0;
	kATSFontFamilyRefUnspecified = 0;
	kATSFontRefUnspecified		= 0;


type
	ATSFontMetricsPtr = ^ATSFontMetrics;
	ATSFontMetrics = record
		version:				UInt32;
		ascent:					Float32;								{  Maximum height above baseline reached by the glyphs in the font  }
																		{  or maximum distance to the right of the centerline reached by the glyphs in the font  }
		descent:				Float32;								{  Maximum depth below baseline reached by the glyphs in the font  }
																		{  or maximum distance to the left of the centerline reached by the glyphs in the font  }
		leading:				Float32;								{  Desired spacing between lines of text  }
		avgAdvanceWidth:		Float32;
		maxAdvanceWidth:		Float32;								{  Maximum advance width or height of the glyphs in the font  }
		minLeftSideBearing:		Float32;								{  Minimum left or top side bearing  }
		minRightSideBearing:	Float32;								{  Minimum right or bottom side bearing  }
		stemWidth:				Float32;								{  Width of the dominant vertical stems of the glyphs in the font  }
		stemHeight:				Float32;								{  Vertical width of the dominant horizontal stems of glyphs in the font  }
		capHeight:				Float32;								{  Height of a capital letter from the baseline to the top of the letter  }
		xHeight:				Float32;								{  Height of lowercase characters in a font, specifically the letter x, excluding ascenders and descenders  }
		italicAngle:			Float32;								{  Angle in degrees counterclockwise from the vertical of the dominant vertical strokes of the glyphs in the font  }
		underlinePosition:		Float32;								{  Distance from the baseline for positioning underlining strokes  }
		underlineThickness:		Float32;								{  Stroke width for underlining  }
	end;


const
	kATSItalicQDSkew			= $00004000;					{  fixed value of 0.25  }
	kATSBoldQDStretch			= $00018000;					{  fixed value of 1.50  }
	kATSRadiansFactor			= 1144;							{  fixed value of approx. pi/180 (0.0174560546875)  }

	{	 Glyph outline path constants used in ATSFontGetNativeCurveType. 	}

type
	ATSCurveType 				= UInt16;
const
	kATSCubicCurveType			= $0001;
	kATSQuadCurveType			= $0002;
	kATSOtherCurveType			= $0003;

{ 
    This is what the ATSGlyphRef is set to when the glyph is deleted -
    that is, when the glyph is set to no longer appear when the layout
    is actually drawn
}
const
  kATSDeletedGlyphcode          = $FFFF;


type
	ATSUCurvePathPtr = ^ATSUCurvePath;
	ATSUCurvePath = record
		vectors:				UInt32;
		controlBits:			array [0..0] of UInt32;
		vector:					array [0..0] of Float32Point;
	end;

	ATSUCurvePathsPtr = ^ATSUCurvePaths;
	ATSUCurvePaths = record
		contours:				UInt32;
		contour:				array [0..0] of ATSUCurvePath;
	end;

	{	 Glyph ideal metrics 	}
	ATSGlyphIdealMetricsPtr = ^ATSGlyphIdealMetrics;
	ATSGlyphIdealMetrics = record
		advance:				Float32Point;
		sideBearing:			Float32Point;
		otherSideBearing:		Float32Point;
	end;

	{	 Glyph screen metrics 	}
	ATSGlyphScreenMetricsPtr = ^ATSGlyphScreenMetrics;
	ATSGlyphScreenMetrics = record
		deviceAdvance:			Float32Point;
		topLeft:				Float32Point;
		height:					UInt32;
		width:					UInt32;
		sideBearing:			Float32Point;
		otherSideBearing:		Float32Point;
	end;

	GlyphID								= ATSGlyphRef;
	GlyphID_fix	= GlyphID; { used as field type when a record declaration contains a GlyphID field identifier }
	GlyphIDPtr = ^GlyphID;
{$ALIGN MAC68K}


end.

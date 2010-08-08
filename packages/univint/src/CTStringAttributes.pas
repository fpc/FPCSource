{
 *	CTStringAttributes.h
 *	CoreText
 *
 *	Copyright (c) 2004-2008 Apple Inc. All rights reserved.
 *
 }
{       Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CTStringAttributes;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses MacTypes,CFBase,CFNumber,CFString,CGColor;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ The purpose of this file is to define all the attributes to which
	CoreText will respond when placed in a CFAttributedString. These
	are left out of other header file on purpose in order to avoid
	layering problems. This file is allowed to include any other header
	file it wants to. }

{ --------------------------------------------------------------------------- }
{ CFAttributedStringRef Attribute Prototypes }
{ --------------------------------------------------------------------------- }

{!
	@const		kCTFontAttributeName
	@abstract	The font.

	@discussion Value must be a CTFontRef. Default is Helvetica 12.
}

var kCTFontAttributeName: CFStringRef; external name '_kCTFontAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTForegroundColorFromContextAttributeName
	@abstract	Never set a foreground color in the CGContext; use what is set as
				the context's fill color.

	@discussion Value must be a CFBooleanRef. Default is false. The reason
				why this exists is because an NSAttributedString defaults to a
				black color if no color attribute is set. This forces CoreText to
				set the color in the context. This will allow developers to
				sidestep this, making CoreText set nothing but font information
				in the CGContext. If set, this attribute also determines the
				color used by kCTUnderlineStyleAttributeName, in which case it
				overrides the foreground color.
}

var kCTForegroundColorFromContextAttributeName: CFStringRef; external name '_kCTForegroundColorFromContextAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTKernAttributeName
	@abstract	A kerning adjustment.

	@discussion Value must be a CFNumberRef float. Default is standard kerning.
				The kerning attribute indicate how many points the following
				character should be shifted from its default offset as defined
				by the current character's font in points; a positive kern
				indicates a shift farther along and a negative kern indicates a
				shift closer to the current character. If this attribute is not
				present, standard kerning will be used. If this attribute is
				set to 0.0, no kerning will be done at all.
}

var kCTKernAttributeName: CFStringRef; external name '_kCTKernAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTLigatureAttributeName
	@abstract	Controls ligature formation.

	@discussion Value must be a CFNumberRef. Default is int value 1. The ligature
				attribute determines what kinds of ligatures should be used when
				displaying the string. A value of 0 indicates that only ligatures
				essential for proper rendering of text should be used, 1
				indicates that standard ligatures should be used, and 2 indicates
				that all available ligatures should be used. Which ligatures are
				standard depends on the script and possibly the font. Arabic
				text, for example, requires ligatures for many character
				sequences, but has a rich set of additional ligatures that
				combine characters. English text has no essential ligatures, and
				typically has only two standard ligatures, those for "fi" and
				"fl" -- all others being considered more advanced or fancy.
}

var kCTLigatureAttributeName: CFStringRef; external name '_kCTLigatureAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTForegroundColorAttributeName
	@abstract	The foreground color.

	@discussion Value must be a CGColorRef. Default value is black.
}

var kCTForegroundColorAttributeName: CFStringRef; external name '_kCTForegroundColorAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTParagraphStyleAttributeName
	@abstract	A CTParagraphStyle object which is used to specify things like
				line alignment, tab rulers, writing direction, etc.

	@discussion Value must be a CTParagraphStyleRef. Default is an empty
				CTParagraphStyle object. See CTParagraphStyle.h for more
				information.
}

var kCTParagraphStyleAttributeName: CFStringRef; external name '_kCTParagraphStyleAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTStrokeWidthAttributeName
	@abstract	The stroke width.

	@discussion Value must be a CFNumberRef. Default value is 0.0, or no stroke.
				This attribute, interpreted as a percentage of font point size,
				controls the text drawing mode: positive values effect drawing
				with stroke only; negative values are for stroke and fill. A
				typical value for outlined text is 3.0.
}

var kCTStrokeWidthAttributeName: CFStringRef; external name '_kCTStrokeWidthAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{!
	@const		kCTStrokeColorAttributeName
	@abstract	The stroke color.

	@discussion Value must be a CGColorRef. Default is the foreground color.
}

var kCTStrokeColorAttributeName: CFStringRef; external name '_kCTStrokeColorAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{!
	@const		kCTUnderlineStyleAttributeName
	@abstract	Allows the setting of an underline to be applied at render
				time.

	@discussion Value must be a CFNumberRef. Default is kCTUnderlineStyleNone.
				Set a value of something other than kCTUnderlineStyleNone to draw
				an underline. In addition, the CTUnderlineStyleModifiers can be
				used to modify the look of the underline. The underline color
				will be determined by the text's foreground color.
}

var kCTUnderlineStyleAttributeName: CFStringRef; external name '_kCTUnderlineStyleAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTSuperscriptAttributeName
	@abstract	Controls vertical text positioning.

	@discussion Value must be a CFNumberRef. Default is int value 0. If supported
				by the specified font, a value of 1 enables superscripting and a
				value of -1 enables subscripting.
}

var kCTSuperscriptAttributeName: CFStringRef; external name '_kCTSuperscriptAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTUnderlineColorAttributeName
	@abstract	The underline color.

	@discussion Value must be a CGColorRef. Default is the foreground color.
}

var kCTUnderlineColorAttributeName: CFStringRef; external name '_kCTUnderlineColorAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTVerticalFormsAttributeName
	@abstract	Controls glyph orientation.

	@discussion	Value must be a CFBooleanRef. Default is false. A value of false
				indicates that horizontal glyph forms are to be used, true
				indicates that vertical glyph forms are to be used.
}

var kCTVerticalFormsAttributeName: CFStringRef; external name '_kCTVerticalFormsAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTGlyphInfoAttributeName
	@abstract	Allows the use of unencoded glyphs.

	@discussion Value must be a CTGlyphInfoRef. The glyph specified by this
				CTGlyphInfo object is assigned to the entire attribute range,
				provided that its contents match the specified base string and
				that the specified glyph is available in the font specified by
				kCTFontAttributeName. See CTGlyphInfo.h for more information.
}

var kCTGlyphInfoAttributeName: CFStringRef; external name '_kCTGlyphInfoAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@const		kCTCharacterShapeAttributeName
	@abstract	Controls glyph selection.

	@discussion	Value must be a CFNumberRef. Default is value is 0 (disabled).
				A non-zero value is interpreted as Apple Type Services
				kCharacterShapeType selector + 1; see <ATS/SFNTLayoutTypes.h>
				for selectors. For example, an attribute value of 1 corresponds
				to kTraditionalCharactersSelector.
}

var kCTCharacterShapeAttributeName: CFStringRef; external name '_kCTCharacterShapeAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@enum		CTUnderlineStyle
	@abstract	Underline style specifiers.

	@discussion These underline type specifiers can be applied to the value set
				with the kCTUnderlineStyleAttributeName attribute to tell
				CoreText that you want a different underline style.
}

const
	kCTUnderlineStyleNone = $00;
	kCTUnderlineStyleSingle = $01;
	kCTUnderlineStyleThick = $02;
	kCTUnderlineStyleDouble = $09;
type
	CTUnderlineStyle = SInt32;


{!
	@enum		CTUnderlineStyleModifiers
	@abstract	Underline style modifiers.

	@discussion Set these bits with the CTUnderlineStyle that you set with the
				kCTUnderlineStyleAttributeName attribute to modify how the
				underline will be drawn.
}

const
	kCTUnderlinePatternSolid = $0000;
	kCTUnderlinePatternDot = $0100;
	kCTUnderlinePatternDash = $0200;
	kCTUnderlinePatternDashDot = $0300;
	kCTUnderlinePatternDashDotDot = $0400;
type
	CTUnderlineStyleModifiers = SInt32;

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

{ CoreGraphics - CGFont.h
 * Copyright (c) 1999-2003 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGFont;
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
uses MacTypes,CFBase,CFData,CFDictionary,CFArray,CGBase;
{$ALIGN POWER}


{ The type used to represent a CoreGraphics font. }

type
	CGFontRef = ^SInt32; { an opaque 32-bit type }

{ A type to represent indexes in a CGFontRef. }

type
	CGFontIndex = UInt16;

{ A type to represent glyph identifiers in a CGFontRef. }

type
	CGGlyph = CGFontIndex;
	CGGlyphPtr								= ^CGGlyph;

{ The format of a PostScript font subset. Type1 is documented in the
 * "Adobe Type 1 Font Format"; Type3 in the "PostScript Language Reference,
 * 3rd ed." and Type42 in "Adobe Technical Note 5012, The Type 42 Font
 * Format Specification". }

type
	CGFontPostScriptFormat = SInt32;
const
	kCGFontPostScriptFormatType1 = 1;
	kCGFontPostScriptFormatType3 = 3;
	kCGFontPostScriptFormatType42 = 42;


const
{ The maximum allowed value of a CGFontIndex. Always <= USHRT_MAX - 1. }
	kCGFontIndexMax = (1 shl 16) - 2;

    { A value representing an invalid CGFontIndex. Always <= USHRT_MAX. }
	kCGFontIndexInvalid = (1 shl 16) - 1;

    { The maximum allowed value of a CGGlyph. }
	kCGGlyphMax = kCGFontIndexMax;


{ Return the CFTypeID for CGFontRefs. }

function CGFontGetTypeID: CFTypeID; external name '_CGFontGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a CGFontRef using `platformFontReference', a pointer to a
 * platform-specific font reference.  For MacOS X, `platformFontReference'
 * should be a pointer to an ATSFontRef. }

function CGFontCreateWithPlatformFont( platformFontReference: UnivPtr ): CGFontRef; external name '_CGFontCreateWithPlatformFont';

{ Return a font based on `font' with the variation specification
 * dictionary `variations' applied to `font'. A variation specification
 * dictionary contains keys corresponding the variation axis names of the
 * font.  Each key is a variation axis name; the value for each key is the
 * value specified for that particular variation axis represented as a
 * CFNumberRef.  If a variation axis name is not specified in `variations',
 * then the current value from `font' is used. }

function CGFontCreateCopyWithVariations( font: CGFontRef; variations: CFDictionaryRef ): CGFontRef; external name '_CGFontCreateCopyWithVariations'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Equivalent to `CFRetain(font)', except it doesn't crash (as CFRetain
 * does) if `font' is NULL. }

function CGFontRetain( font: CGFontRef ): CGFontRef; external name '_CGFontRetain';

{ Equivalent to `CFRelease(font)', except it doesn't crash (as CFRelease
 * does) if `font' is NULL. }

procedure CGFontRelease( font: CGFontRef ); external name '_CGFontRelease';

{ Return the PostScript name of `font'. }

function CGFontCopyPostScriptName( font: CGFontRef ): CFStringRef; external name '_CGFontCopyPostScriptName'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return an array of the variation axis dictionaries for `font'. Each
 * variation axis dictionary contains values for the variation axis keys
 * listed below. This function returns NULL if `font' doesn't support
 * variations. }

function CGFontCopyVariationAxes( font: CGFontRef ): CFArrayRef; external name '_CGFontCopyVariationAxes'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the variation specification dictionary from `font'. This
 * dictionary contains keys corresponding the variation axis names of the
 * font.  Each key is a variation axis name; the value for each key is the
 * value specified for that particular variation axis represented as a
 * CFNumberRef. This function returns NULL if `font' doesn't support
 * variations. }

function CGFontCopyVariations( font: CGFontRef ): CFDictionaryRef; external name '_CGFontCopyVariations'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return true if a subset in the PostScript format `format' can be created
 * for `font'; false otherwise. }

function CGFontCanCreatePostScriptSubset( font: CGFontRef; format: CGFontPostScriptFormat ): CBool; external name '_CGFontCanCreatePostScriptSubset'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a subset of `font' named `subsetName' in the PostScript format
 * `format'.  The subset will contain the glyphs specified by `glyphs', an
 * array of `count' CGGlyphs.  If non-NULL, `encoding' specifies the
 * default encoding for the subset. }

type
	CGGlyph256Array = array[0..255] of CGGlyph;
function CGFontCreatePostScriptSubset( font: CGFontRef; subsetName: CFStringRef; format: CGFontPostScriptFormat; {const} glyphs: {variable-size-array} CGGlyphPtr; count: size_t; const (*var*) encoding: CGGlyph256Array ): CFDataRef; external name '_CGFontCreatePostScriptSubset'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return a PostScript encoding of `font' containing glyphs in `encoding'. }

function CGFontCreatePostScriptEncoding( font: CGFontRef; const (*var*) encoding: CGGlyph256Array ): CFDataRef; external name '_CGFontCreatePostScriptEncoding'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{** Keys for the font variation axis dictionary. **}

{ The key used to obtain the variation axis name from a variation axis
 * dictionary. The value obtained with this key is a CFStringRef specifying
 * the name of the variation axis. }

var kCGFontVariationAxisName: CFStringRef; external name '_kCGFontVariationAxisName'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The key used to obtain the minimum variation axis value from a variation
 * axis dictionary. The value obtained with this key is a CFNumberRef
 * specifying the minimum value of the variation axis. }

var kCGFontVariationAxisMinValue: CFStringRef; external name '_kCGFontVariationAxisMinValue'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The key used to obtain the maximum variation axis value from a variation
 * axis dictionary. The value obtained with this key is a CFNumberRef
 * specifying the maximum value of the variation axis. }

var kCGFontVariationAxisMaxValue: CFStringRef; external name '_kCGFontVariationAxisMaxValue'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The key used to obtain the default variation axis value from a variation
 * axis dictionary. The value obtained with this key is a CFNumberRef
 * specifying the default value of the variation axis. }

var kCGFontVariationAxisDefaultValue: CFStringRef; external name '_kCGFontVariationAxisDefaultValue'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Obsolete; don't use these. }

const
	CGGlyphMin = 0;
	CGGlyphMax = kCGGlyphMax;


end.

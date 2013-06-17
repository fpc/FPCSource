{
 *	CTFramesetter.h
 *	CoreText
 *
 *	Copyright (c) 2003-2012 Apple Inc. All rights reserved.
 *
 }
{  Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CTFramesetter;
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,CTFrame,CTTypesetter,CFBase,CFDictionary,CFAttributedString,CGGeometry,CGPath;
{$endc} {not MACOSALLINCLUDE}


{$ALIGN POWER}


{!
    @header

    Thread Safety Information

    All functions in this header are thread safe unless otherwise specified.
}


{ --------------------------------------------------------------------------- }
{ Framesetter Types }
{ --------------------------------------------------------------------------- }

type
	CTFramesetterRef = ^__CTFramesetter; { an opaque type }
	__CTFramesetter = record end;
	CTFramesetterRefPtr = ^CTFramesetterRef;


{!
	@function	CTFramesetterGetTypeID
	@abstract	Returns the CFType of the framesetter object
}

function CTFramesetterGetTypeID: CFTypeID; external name '_CTFramesetterGetTypeID';
(* CT_AVAILABLE_STARTING( __MAC_10_5, __IPHONE_3_2) *)


{ --------------------------------------------------------------------------- }
{ Framesetter Creation }
{ --------------------------------------------------------------------------- }

{!
	@function	CTFramesetterCreateWithAttributedString
	@abstract	Creates an immutable framesetter object from an attributed
				string.

	@discussion The resultant framesetter object can be used to create and
				fill text frames with the CTFramesetterCreateFrame call.

	@param		string
				The run with which you want to construct the framesetter
				object with.

	@result		This function will return a reference to a CTFramesetter if
				the call was successful. Otherwise, it will return NULL.
}

function CTFramesetterCreateWithAttributedString( strng: CFAttributedStringRef ): CTFramesetterRef; external name '_CTFramesetterCreateWithAttributedString';
(* CT_AVAILABLE_STARTING( __MAC_10_5, __IPHONE_3_2) *)


{ --------------------------------------------------------------------------- }
{ Frame Creation }
{ --------------------------------------------------------------------------- }

{!
	@function	CTFramesetterCreateFrame
	@abstract	Creates an immutable frame from a framesetter.

	@discussion This call will create a frame full of glyphs in the shape of
				the path provided by the "path" parameter. The framesetter
				will continue to fill the frame until it either runs out of
				text or it finds that text no longer fits.

	@param		framesetter
				The framesetter that will be used to create the frame.

	@param		stringRange
				The string range which the new frame will be based on. The
				string range is a range over the string that was used to
				create the framesetter. If the length portion of the range
				is set to 0, then the framesetter will continue to add lines
				until it runs out of text or space.

	@param		path
				A CGPath object that specifies the shape which the frame will
				take on.  Accepts only rectangles up to and including Mac OS X 10.6.

	@param		frameAttributes
				Additional attributes that control the frame filling process
				can be specified here, or NULL if there are no such attributes.
				See CTFrame.h for available attributes.

	@result		This function will return a reference to a new CTFrame object
				if the call was successful. Otherwise, it will return NULL.
}

function CTFramesetterCreateFrame( framesetter: CTFramesetterRef; stringRange: CFRange; path: CGPathRef; frameAttributes: CFDictionaryRef ): CTFrameRef; external name '_CTFramesetterCreateFrame';
(* CT_AVAILABLE_STARTING( __MAC_10_5, __IPHONE_3_2) *)


{!
	@function	CTFramesetterGetTypesetter
	@abstract	Returns the typesetter object being used by the framesetter.

	@discussion Each framesetter uses a typesetter internally to perform
				line breaking and other contextual analysis based on the
				characters in a string; this function returns the typesetter
				being used by a particular framesetter if the caller would
				like to perform other operations on that typesetter.

	@param		framesetter
				The framesetter from which a typesetter is being requested.

	@result		This function will return a reference to a CTTypesetter
				object if the call was successful. Otherwise, it will return
				NULL. The framesetter maintains a reference to the returned
				object, which should not be disposed by the caller.
}

function CTFramesetterGetTypesetter( framesetter: CTFramesetterRef ): CTTypesetterRef; external name '_CTFramesetterGetTypesetter';
(* CT_AVAILABLE_STARTING( __MAC_10_5, __IPHONE_3_2) *)


{ --------------------------------------------------------------------------- }
{ Frame Sizing }
{ --------------------------------------------------------------------------- }

{!
	@function	CTFramesetterSuggestFrameSizeWithConstraints
	@abstract	Determines the frame size needed for a string range.

	@discussion	This function may be used to determine how much space is needed
				to display a string, optionally by constraining the space along
				either dimension.

	@param		framesetter
				The framesetter that will be used for measuring the frame size.

	@param		stringRange
				The string range to which the frame size will apply. The
				string range is a range over the string that was used to
				create the framesetter. If the length portion of the range
				is set to 0, then the framesetter will continue to add lines
				until it runs out of text or space.

	@param		frameAttributes
				Additional attributes that control the frame filling process
				can be specified here, or NULL if there are no such attributes.

	@param		constraints
				The width and height to which the frame size will be constrained,
				A value of CGFLOAT_MAX for either dimension indicates that it
				should be treated as unconstrained.

	@param		fitRange
				The range of the string that actually fit in the constrained size.

	@result		The actual dimensions for the given string range and constraints.
}

function CTFramesetterSuggestFrameSizeWithConstraints( framesetter: CTFramesetterRef; stringRange: CFRange; frameAttributes: CFDictionaryRef; constraints: CGSize; var fitRange: CFRange ): CGSize; external name '_CTFramesetterSuggestFrameSizeWithConstraints';
(* CT_AVAILABLE_STARTING( __MAC_10_5, __IPHONE_3_2) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

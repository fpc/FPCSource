{
 *	CTGlyphInfo.h
 *	CoreText
 *
 *	Copyright (c) 2006-2008 Apple Inc. All rights reserved.
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

unit CTGlyphInfo;
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
uses MacTypes,CTFont,CFBase,CGFont;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{!
    @header

    Thread Safety Information

    All functions in this header are thread safe unless otherwise specified.
}


{ --------------------------------------------------------------------------- }
{ Glyph Info Types }
{ --------------------------------------------------------------------------- }

type
	CTGlyphInfoRef = ^SInt32; { an opaque type }
	CTGlyphInfoRefPtr = ^CTGlyphInfoRef;

{!
	@function	CTGlyphInfoGetTypeID
	@abstract	Returns the CFType of the glyph info object
}

function CTGlyphInfoGetTypeID: CFTypeID; external name '_CTGlyphInfoGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ --------------------------------------------------------------------------- }
{ Glyph Info Values }
{ --------------------------------------------------------------------------- }

{!
	@enum		CTCharacterCollection
	@abstract	These constants specify character collections.

	@constant	kCTIdentityMappingCharacterCollection
				Indicates that the character identifier is equal to the CGGlyph
				glyph index.

	@constant	kCTAdobeCNS1CharacterCollection
				Indicates the Adobe-CNS1 mapping.

	@constant	kCTAdobeGB1CharacterCollection
				Indicates the Adobe-GB1 mapping.

	@constant	kCTAdobeJapan1CharacterCollection
				Indicates the Adobe-Japan1 mapping.

	@constant	kCTAdobeJapan2CharacterCollection
				Indicates the Adobe-Japan2 mapping.

	@constant	kCTAdobeKorea1CharacterCollection
				Indicates the Adobe-Korea1 mapping.
}

const
	kCTIdentityMappingCharacterCollection = 0;
	kCTAdobeCNS1CharacterCollection = 1;
	kCTAdobeGB1CharacterCollection = 2;
	kCTAdobeJapan1CharacterCollection = 3;
	kCTAdobeJapan2CharacterCollection = 4;
	kCTAdobeKorea1CharacterCollection = 5;
type
	CTCharacterCollection = UInt16;


{ --------------------------------------------------------------------------- }
{ Glyph Info Creation }
{ --------------------------------------------------------------------------- }

{!
	@function	CTGlyphInfoCreateWithGlyphName
	@abstract	Creates an immutable glyph info object.

	@discussion This function creates an immutable glyph info object for a glyph
				name such as "copyright" and a specified font.

	@param		glyphName
				The name of the glyph.

	@param		font
				The font to be associated with the returned CTGlyphInfo object.

	@param		baseString
				The part of the string the returned object is intended
				to override.

	@result		If glyph info creation was successful, this function will return
				a valid reference to an immutable CTGlyphInfo object. Otherwise,
				this function will return NULL.
}

function CTGlyphInfoCreateWithGlyphName( glyphName: CFStringRef; font: CTFontRef; baseString: CFStringRef ): CTGlyphInfoRef; external name '_CTGlyphInfoCreateWithGlyphName';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTGlyphInfoCreateWithGlyph
	@abstract	Creates an immutable glyph info object.

	@discussion This function creates an immutable glyph info object for a glyph
				index and a specified font.

	@param		glyph
				The glyph identifier.

	@param		font
				The font to be associated with the returned CTGlyphInfo object.

	@param		baseString
				The part of the string the returned object is intended
				to override.

	@result		If glyph info creation was successful, this function will return
				a valid reference to an immutable CTGlyphInfo object. Otherwise,
				this function will return NULL.
}

function CTGlyphInfoCreateWithGlyph( glyph: CGGlyph; font: CTFontRef; baseString: CFStringRef ): CTGlyphInfoRef; external name '_CTGlyphInfoCreateWithGlyph';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTGlyphInfoCreateWithCharacterIdentifier
	@abstract	Creates an immutable glyph info object.

	@discussion This function creates an immutable glyph info object for a
				character identifier and a character collection.

	@param		cid
				A character identifier.

	@param		collection
				A character collection identifier.

	@param		baseString
				The part of the string the returned object is intended
				to override.

	@result		If glyph info creation was successful, this function will return
				a valid reference to an immutable CTGlyphInfo object. Otherwise,
				this function will return NULL.
}

function CTGlyphInfoCreateWithCharacterIdentifier( cid: CGFontIndex; collection: CTCharacterCollection; baseString: CFStringRef ): CTGlyphInfoRef; external name '_CTGlyphInfoCreateWithCharacterIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ --------------------------------------------------------------------------- }
{ Glyph Info Access }
{ --------------------------------------------------------------------------- }

{!
	@function	CTGlyphInfoGetGlyphName
	@abstract	Gets the glyph name for a glyph info, if applicable.

	@discussion This function will return the glyph name.

	@param		glyphInfo
				The glyph info for which you would like the glyph name. This
				parameter may not be set to NULL.

	@result		If the glyph info object was created with a glyph name, it will
				be returned. Otherwise, this function will return NULL.
}

function CTGlyphInfoGetGlyphName( glyphInfo: CTGlyphInfoRef ): CFStringRef; external name '_CTGlyphInfoGetGlyphName';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTGlyphInfoGetCharacterIdentifier
	@abstract	Gets the character identifier for a glyph info.

	@discussion This function will return the character identifier.

	@param		glyphInfo
				The glyph info for which you would like the character identifier.
				This parameter may not be set to NULL.

	@result		If the glyph info object was created with a character identifier,
				it will be returned. Otherwise, this function will return 0.
}

function CTGlyphInfoGetCharacterIdentifier( glyphInfo: CTGlyphInfoRef ): CGFontIndex; external name '_CTGlyphInfoGetCharacterIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTGlyphInfoGetCharacterCollection
	@abstract	Gets the character collection for a glyph info.

	@discussion This function will return the character collection. If the glyph
				info object was created with a glyph name or a glyph index, its
				character collection will be
				kCTIdentityMappingCharacterCollection.

	@param		glyphInfo
				The glyph info for which you would like the character collection.
				This parameter may not be set to NULL.

	@result		This function will return the character collection of the given
				glyph info.
}

function CTGlyphInfoGetCharacterCollection( glyphInfo: CTGlyphInfoRef ): CTCharacterCollection; external name '_CTGlyphInfoGetCharacterCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

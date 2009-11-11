{
 *	CTTextTab.h
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

unit CTTextTab;
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
uses MacTypes,CTParagraphStyle,CFBase,CFCharacterSet,CFDictionary;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{!
    @header

    Thread Safety Information

    All functions in this header are thread safe unless otherwise specified.
}


{ --------------------------------------------------------------------------- }
{ Text Tab Types }
{ --------------------------------------------------------------------------- }

{ A CTTextTab represents a tab in an CTParagraphStyle object, storing an
	alignment type and location.

	CoreText supports four alignment types: left, center, right, and decimal.
	These alignment types are absolute, not based on the line sweep direction
	of text. For example, tabbed text is always positioned to the left of a
	right-aligned tab, whether the line sweep direction is left to right or right
	to left. A tab's location, on the other hand, is relative to the back margin.
	A tab set at 1.5", for example, is at 1.5" from the right in right to left
	text.
}

type
	CTTextTabRef = ^SInt32; { an opaque type }


{!
	@function	CTTypesetterGetTypeID
	@abstract	Returns the CFType of the text tab object
}

function CTTextTabGetTypeID: CFTypeID; external name '_CTTextTabGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ --------------------------------------------------------------------------- }
{ Text Tab Constants }
{ --------------------------------------------------------------------------- }

{!
	@const		kCTTabColumnTerminatorsAttributeName
	@abstract	Used to specify the terminating character for a tab column

	@discussion The value associated with this attribute is a CFCharacterSet. The
				character set is used to determine the terminating character for
				a tab column. The tab and newline characters are implied even if
				they don't exist in the character set. This attribute can be used
				to implement decimal tabs, for instance. This attribute is
				optional.
}

var kCTTabColumnTerminatorsAttributeName: CFStringRef; external name '_kCTTabColumnTerminatorsAttributeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ --------------------------------------------------------------------------- }
{ Text Tab Creation }
{ --------------------------------------------------------------------------- }

{!
	@function	CTTextTabCreate
	@abstract	Creates and initializes a new text tab.

	@param		alignment
				The tab's alignment. This is used to determine the position of
				text inside the tab column. This parameter must be set to a valid
				CTTextAlignment value or this function will return NULL.

	@param		location
				The tab's ruler location, relative to the back margin.

	@param		options
				Options to pass in when the tab is created. Currently, the only
				option available is kCTTabColumnTerminatorsAttributeName. This
				parameter is optional and can be set to NULL if not needed.

	@result		This function will return a reference to a CTTextTab if the call
				was successful. Otherwise, this function will return NULL.
}

function CTTextTabCreate( alignment: CTTextAlignment; location: Float64; options: CFDictionaryRef ): CTTextTabRef; external name '_CTTextTabCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ --------------------------------------------------------------------------- }
{ Text Tab Access }
{ --------------------------------------------------------------------------- }

{!
	@function	CTTextTabGetAlignment
	@abstract	Returns the text alignment of the tab.

	@param		tab
				The tab whose text alignment you wish to access.

	@result		The tab's text alignment value.
}

function CTTextTabGetAlignment( tab: CTTextTabRef ): CTTextAlignment; external name '_CTTextTabGetAlignment';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTTextTabGetLocation
	@abstract	Returns the tab's ruler location.

	@param		tab
				The tab whose location you wish to access.

	@result		The tab's ruler location relative to the back margin.
}

function CTTextTabGetLocation( tab: CTTextTabRef ): Float64; external name '_CTTextTabGetLocation';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{!
	@function	CTTextTabGetOptions
	@abstract	Returns the dictionary of attributes associated with the tab. 

	@param		tab
				The tab whose attributes you wish to access.

	@result		The dictionary of attributes associated with the tab or NULL if
				no dictionary is present.
}

function CTTextTabGetOptions( tab: CTTextTabRef ): CFDictionaryRef; external name '_CTTextTabGetOptions';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

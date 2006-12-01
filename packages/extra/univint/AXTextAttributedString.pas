{
 *  AXTextAttributedString.h
 *
 *  Copyright (c) 2002 Apple Computer, Inc. All rights reserved.
 *
 }

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit AXTextAttributedString;
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
uses MacTypes, CFBase;
{$ALIGN POWER}

var kAXFontTextAttribute: CFStringRef; external name '_kAXFontTextAttribute'; (* attribute const *)		// CFDictionaryRef - see kAXFontTextAttribute keys below
var kAXForegroundColorTextAttribute: CFStringRef; external name '_kAXForegroundColorTextAttribute'; (* attribute const *)	// CGColorRef
var kAXBackgroundColorTextAttribute: CFStringRef; external name '_kAXBackgroundColorTextAttribute'; (* attribute const *)	// CGColorRef
var kAXUnderlineColorTextAttribute: CFStringRef; external name '_kAXUnderlineColorTextAttribute'; (* attribute const *)	// CGColorRef
var kAXStrikethroughColorTextAttribute: CFStringRef; external name '_kAXStrikethroughColorTextAttribute'; (* attribute const *)	// CGColorRef
var kAXUnderlineTextAttribute: CFStringRef; external name '_kAXUnderlineTextAttribute'; (* attribute const *)		// CFNumberRef - AXUnderlineStyle
var kAXSuperscriptTextAttribute: CFStringRef; external name '_kAXSuperscriptTextAttribute'; (* attribute const *)		// CFNumberRef = + number for superscript - for subscript
var kAXStrikethroughTextAttribute: CFStringRef; external name '_kAXStrikethroughTextAttribute'; (* attribute const *)	// CFBooleanRef
var kAXShadowTextAttribute: CFStringRef; external name '_kAXShadowTextAttribute'; (* attribute const *)		// CFBooleanRef

var kAXAttachmentTextAttribute: CFStringRef; external name '_kAXAttachmentTextAttribute'; (* attribute const *)		// AXUIElementRef
var kAXLinkTextAttribute: CFStringRef; external name '_kAXLinkTextAttribute'; (* attribute const *)		// AXUIElementRef

var kAXNaturalLanguageTextAttribute: CFStringRef; external name '_kAXNaturalLanguageTextAttribute'; (* attribute const *)	// CFStringRef - the spoken language of the text
var kAXReplacementStringTextAttribute: CFStringRef; external name '_kAXReplacementStringTextAttribute'; (* attribute const *)	// CFStringRef

var kAXMisspelledTextAttribute: CFStringRef; external name '_kAXMisspelledTextAttribute'; (* attribute const *)		// AXUIElementRef

// kAXFontTextAttribute keys
var kAXFontNameKey: CFStringRef; external name '_kAXFontNameKey'; (* attribute const *)		// CFStringRef - required
var kAXFontFamilyKey: CFStringRef; external name '_kAXFontFamilyKey'; (* attribute const *)		// CFStringRef - not required
var kAXVisibleNameKey: CFStringRef; external name '_kAXVisibleNameKey'; (* attribute const *)		// CFStringRef - not required
var kAXFontSizeKey: CFStringRef; external name '_kAXFontSizeKey'; (* attribute const *)		// CFNumberRef - required

const
	kAXUnderlineStyleNone = $0;
	kAXUnderlineStyleSingle = $1;
	kAXUnderlineStyleThick = $2;
	kAXUnderlineStyleDouble = $9;
type
	AXUnderlineStyle = UInt32;


// DO NOT USE. This is an old, misspelled version of one of the above constants.
var kAXForegoundColorTextAttribute: CFStringRef; external name '_kAXForegoundColorTextAttribute'; (* attribute const *)	// CGColorRef

end.

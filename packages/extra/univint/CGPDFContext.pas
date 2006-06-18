{ CoreGraphics - CGPDFContext.h
 * Copyright (c) 2000-2004 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGPDFContext;
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
uses MacTypes,CGGeometry,CFBase,CFDictionary,CFURL,CGBase,CGContext,CGDataConsumer;
{$ALIGN POWER}


{ Create a PDF context, using `consumer' for output. `mediaBox' is the
 * default page media bounding box; if NULL, then a default page size is
 * used. `auxiliaryInfo' specifies additional information used by the PDF
 * context when generating the PDF file.  The keys and values in
 * `auxiliaryInfo' are described below. If `mediaBox' is non-NULL, then its
 * value overrides the value of `kCGPDFContextMediaBox' if specified in the
 * `auxiliaryInfo' dictionary. }

function CGPDFContextCreate( consumer: CGDataConsumerRef; const (*var*) mediaBox: CGRect; auxiliaryInfo: CFDictionaryRef ): CGContextRef; external name '_CGPDFContextCreate';

{ Create a PDF context for writing to `url'. This function behaves in the
 * same manner as the above function, except that the output data will be
 * written to `url'. }

function CGPDFContextCreateWithURL( url: CFURLRef; const (*var*) mediaBox: CGRect; auxiliaryInfo: CFDictionaryRef ): CGContextRef; external name '_CGPDFContextCreateWithURL';

{ Begin a new page in the PDF context `context'. }

procedure CGPDFContextBeginPage( context: CGContextRef; pageInfo: CFDictionaryRef ); external name '_CGPDFContextBeginPage'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ End the current page in the PDF context `context'. }

procedure CGPDFContextEndPage( context: CGContextRef ); external name '_CGPDFContextEndPage'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Set the URL associated with `rect' to `url' in the PDF context
 * `context'. }

procedure CGPDFContextSetURLForRect( context: CGContextRef; url: CFURLRef; rect: CGRect ); external name '_CGPDFContextSetURLForRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a PDF destination named `name' at `point' in the current page of
 * the PDF context `context'. }

procedure CGPDFContextAddDestinationAtPoint( context: CGContextRef; name: CFStringRef; point: CGPoint ); external name '_CGPDFContextAddDestinationAtPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specify a destination named `name' to jump to when clicking in `rect' of
 * the current page of the PDF context `context'. }

procedure CGPDFContextSetDestinationForRect( context: CGContextRef; name: CFStringRef; rect: CGRect ); external name '_CGPDFContextSetDestinationForRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{** Keys for the auxiliary info dictionary or the page info dictionary. **} 

{ The media box for the document or for a given page. Optional; if
 * present, the value of this key must be a CFData containing a CGRect
 * (stored by value, not by reference). }

var kCGPDFContextMediaBox: CFStringRef; external name '_kCGPDFContextMediaBox'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The crop box for the document or for a given page. Optional; if present,
 * the value of this key must be a CFData containing a CGRect (stored by
 * value, not by reference). }

var kCGPDFContextCropBox: CFStringRef; external name '_kCGPDFContextCropBox'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The bleed box for the document or for a given page. Optional; if
 * present, the value of this key must be a CFData containing a CGRect
 * (stored by value, not by reference). }

var kCGPDFContextBleedBox: CFStringRef; external name '_kCGPDFContextBleedBox'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The trim box for the document or for a given page. Optional; if present,
 * the value of this key must be a CFData containing a CGRect (stored by
 * value, not by reference). }

var kCGPDFContextTrimBox: CFStringRef; external name '_kCGPDFContextTrimBox'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The art box for the document or for a given page. Optional; if present,
 * the value of this key must be a CFData containing a CGRect (stored by
 * value, not by reference). }

var kCGPDFContextArtBox: CFStringRef; external name '_kCGPDFContextArtBox'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{** Keys for auxiliary info dictionary. **}

{ The document's title. Optional; if present, the value of this key must
 * be a CFString. }

// const kCGPDFContextTitle: CFStringRef; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The name of the person who created this document. Optional; if present,
 * the value of this key must be a CFString. }

// const kCGPDFContextAuthor: CFStringRef; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The name of the application that created the original data used to
 * create this document. Optional; if present, the value of this key must
 * be a CFString. }

// const kCGPDFContextCreator: CFStringRef; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The "owner password" of the PDF document. If this key is specified, the
 * document will be encrypted using the value as the owner password;
 * otherwise, the document will not be encrypted. The value of this key
 * must be a CFStringRef which can be represented in ASCII encoding; only
 * the first 32 bytes will be used for the password. There is no default
 * value for this key.
 *
 * If the value of this key cannot be represented in ASCII, the document
 * will not be created and the creation function will return NULL. }

var kCGPDFContextOwnerPassword: CFStringRef; external name '_kCGPDFContextOwnerPassword'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The "user password" of the PDF document. If the document is encrypted,
 * then the value of this key will be the user password for the document;
 * if unspecified, the user password will be the empty string. The value of
 * this key must be a CFStringRef which can be represented in ASCII
 * encoding; only the first 32 bytes will be used for the password.
 *
 * If the value of this key cannot be represented in ASCII, the document
 * will not be created and the creation function will return NULL. }

var kCGPDFContextUserPassword: CFStringRef; external name '_kCGPDFContextUserPassword'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Used to specify whether the document allows printing when unlocked with
 * the user password.  The value of this key must be a CFBooleanRef.  The
 * default value of this key is "kCFBooleanTrue". }

var kCGPDFContextAllowsPrinting: CFStringRef; external name '_kCGPDFContextAllowsPrinting'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Used to specify whether the document allows copying when unlocked with
 * the user password.  The value of this key must be a CFBooleanRef.  The
 * default value of this key is "kCFBooleanTrue". }

var kCGPDFContextAllowsCopying: CFStringRef; external name '_kCGPDFContextAllowsCopying'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The document's PDF/X output intent. Optional; if present, the value of
 * this key must be a CFDictionaryRef. The dictionary is added to the
 * /OutputIntents entry in the PDF file's document catalog. The keys and
 * values contained in the dictionary must match those specified in section
 * 9.10.4 of the PDF 1.4 specification, ISO/DIS 15930-3 document published
 * by ISO/TC 130, and Adobe Technical Note #5413. }

// const kCGPDFContextOutputIntent: CFStringRef; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The following keys are supported in the output intent dictionary:
 *
 * kCGPDFXOutputIntentSubtype ("S") - The output intent subtype.  This key
 *    is required; the value of this key must be a CFString equal to
 *    "GTS_PDFX"; otherwise, the dictionary is ignored. }

var kCGPDFXOutputIntentSubtype: CFStringRef; external name '_kCGPDFXOutputIntentSubtype'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ kCGPDFXOutputConditionIdentifier ("OutputConditionIdentifier") - A
 *    string identifying the intended output device or production condition
 *    in a human- or machine-readable form.  This key is required; the
 *    value of this key must be a CFString.  For best results, the string
 *    should be representable losslessly in ASCII encoding. }

var kCGPDFXOutputConditionIdentifier: CFStringRef; external name '_kCGPDFXOutputConditionIdentifier'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ kCGPDFXOutputCondition ("OutputCondition") - A text string identifying
 *    the intended output device or production condition in a human-
 *    readable form.  This key is optional; if present, the value of this
 *    key must be a CFString. }

var kCGPDFXOutputCondition: CFStringRef; external name '_kCGPDFXOutputCondition'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ kCGPDFXRegistryName ("RegistryName") - A string identifying the registry
 *   in which the condition designated by kCGPDFXOutputConditionIdentifier
 *   is defined.  This key is optional; if present, the value of this key
 *   must be a CFString.  For best results, the string should be
 *   representable losslessly in ASCII encoding. }

var kCGPDFXRegistryName: CFStringRef; external name '_kCGPDFXRegistryName'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ kCGPDFXInfo ("Info") - A human-readable text string containing
 *    additional information or comments about the intended target device
 *    or production condition.  This key is required if the value of
 *    kCGPDFXOutputConditionIdentifier does not specify a standard
 *    production condition; it is optional otherwise.  If present, the
 *    value of this key must be a CFString. }

var kCGPDFXInfo: CFStringRef; external name '_kCGPDFXInfo'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ kCGPDFXDestinationOutputProfile ("DestOutputProfile") - An ICC profile
 *    stream defining the transformation from the PDF document's source
 *    colors to output device colorants.  This key is required if the value
 *    of kCGPDFXOutputConditionIdentifier does not specify a standard
 *    production condition; it is optional otherwise.  If present, the
 *    value of this key must be a ICC-based CGColorSpaceRef. }

var kCGPDFXDestinationOutputProfile: CFStringRef; external name '_kCGPDFXDestinationOutputProfile'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The document's output intents. Optional; if present, the value must be a
 * CFArrayRef containing one or more CFDictionaryRefs. The array is added
 * to the PDF document in the /OutputIntents entry in the PDF file's
 * document catalog.  Each dictionary in the array must be of form
 * specified above for the "kCGPDFContextOutputIntent" key, except that
 * only the first dictionary in the array may contain the
 * kCGPDFXOutputIntentSubtype ("S") key with a value of "GTS_PDFX". If both
 * the "kCGPDFContextOutputIntent" and "kCGPDFContextOutputIntents" keys
 * are specified, the former is ignored. }

// const kCGPDFContextOutputIntents: CFStringRef; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Compatibility with earlier versions of Mac OS X. }

// #if MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_4

{
	PNL comments: 
	
	There is an issue here that these types below are macro defines, and they
	conflict with the definitions above unless only one or the other is defined.
	
	This only applies to GPC where GPCMacros.inc contains the macros and 
	is typically read before this Pascal source file.
}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGPDFContextTitle CFSTRP('kCGPDFContextTitle')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGPDFContextAuthor CFSTRP('kCGPDFContextAuthor')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGPDFContextCreator CFSTRP('kCGPDFContextCreator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGPDFContextOutputIntent CFSTRP('kCGPDFContextOutputIntent')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGPDFContextOutputIntents CFSTRP('kCGPDFContextOutputIntents')}
{$endc}

// #endif	{ MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_4 }


end.

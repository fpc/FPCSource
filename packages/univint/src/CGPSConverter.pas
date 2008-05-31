{ CoreGraphics - CGPSConverter.h
 * Copyright (c) 2003 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGPSConverter;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

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
uses MacTypes,CFBase,CFDictionary,CGBase,CGDataConsumer,CGDataProvider;
{$ALIGN POWER}


// CGPSConverterRef defined in CGBase


{ CGPSConverter callbacks.
 *
 * `version' is the version number of the structure passed in as a
 * parameter to the converter creation functions. The structure defined
 * below is version 0.
 *
 * `beginDocument', if non-NULL, is called at the beginning of the
 * conversion of the PostScript document.
 *
 * `endDocument', if non-NULL, is called at the end of conversion of the
 * PostScript document.
 *
 * `beginPage', if non-NULL, is called at the start of the conversion of
 * each page in the PostScript document.
 *
 * `endPage', if non-NULL, is called at the end of the conversion of each
 * page in the PostScript document.
 *
 * `noteProgress', if non-NULL, is called periodically during the
 * conversion to indicate that conversion is proceeding.
 *
 * `noteMessage', if non-NULL, is called to pass any messages that might
 * result during the conversion.
 *
 * `releaseInfo', if non-NULL, is called when the converter is
 * deallocated. }

type
	CGPSConverterBeginDocumentCallback = procedure( info: UnivPtr );

type
	CGPSConverterEndDocumentCallback = procedure( info: UnivPtr; success: CBool );

type
	CGPSConverterBeginPageCallback = procedure( info: UnivPtr; pageNumber: size_t; pageInfo: CFDictionaryRef );

type
	CGPSConverterEndPageCallback = procedure( info: UnivPtr; pageNumber: size_t; pageInfo: CFDictionaryRef );

type
	CGPSConverterProgressCallback = procedure( info: UnivPtr );

type
	CGPSConverterMessageCallback = procedure( info: UnivPtr; message: CFStringRef );

type
	CGPSConverterReleaseInfoCallback = procedure( info: UnivPtr );

type
	CGPSConverterCallbacks = record
		version: UInt32;
		beginDocument: CGPSConverterBeginDocumentCallback;
		endDocument: CGPSConverterEndDocumentCallback;
		beginPage: CGPSConverterBeginPageCallback;
		endPage: CGPSConverterEndPageCallback;
		noteProgress: CGPSConverterProgressCallback;
		noteMessage: CGPSConverterMessageCallback;
		releaseInfo: CGPSConverterReleaseInfoCallback;
	end;

{ Create a CGPSConverter, using `callbacks' to populate its callback
 * table. Each callback will be supplied the `info' value when called. }

function CGPSConverterCreate( info: UnivPtr; const (*var*) callbacks: CGPSConverterCallbacks; options: CFDictionaryRef ): CGPSConverterRef; external name '_CGPSConverterCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Use `converter' to convert PostScript data to PDF data.  The PostScript
 * data is supplied by `provider'; the resulting PDF is written to
 * `consumer'.  Returns true if the conversion succeeded; false
 * otherwise. }

function CGPSConverterConvert( converter: CGPSConverterRef; provider: CGDataProviderRef; consumer: CGDataConsumerRef; options: CFDictionaryRef ): CBool; external name '_CGPSConverterConvert'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Tell the `converter' to abort conversion at the next possible
 * opportunity. }

function CGPSConverterAbort( converter: CGPSConverterRef ): CBool; external name '_CGPSConverterAbort'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return true if `converter' is currently converting data. }

function CGPSConverterIsConverting( converter: CGPSConverterRef ): CBool; external name '_CGPSConverterIsConverting'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the CFTypeID of the CGPSConverter class. }

function CGPSConverterGetTypeID: CFTypeID; external name '_CGPSConverterGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.

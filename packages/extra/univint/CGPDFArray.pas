{ CoreGraphics - CGPDFArray.h
 * Copyright (c) 2002-2004 Apple Computer, Inc. (unpublished)
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

unit CGPDFArray;
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
uses MacTypes,CGPDFObject,CGBase;
{$ALIGN POWER}


// CGPDFArrayRef defined in CGBase


{ Return the number of items in `array'. }

function CGPDFArrayGetCount( arry: CGPDFArrayRef ): size_t; external name '_CGPDFArrayGetCount'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and return the result in
 * `value'. Return true on success; false otherwise. }

function CGPDFArrayGetObject( arry: CGPDFArrayRef; index: size_t; var value: CGPDFObjectRef ): CBool; external name '_CGPDFArrayGetObject'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a null, return
 * true; otherwise, return false. }

function CGPDFArrayGetNull( arry: CGPDFArrayRef; index: size_t ): CBool; external name '_CGPDFArrayGetNull'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a boolean, return
 * the result in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetBoolean( arry: CGPDFArrayRef; index: size_t; var value: CGPDFBoolean ): CBool; external name '_CGPDFArrayGetBoolean'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's an integer, return
 * the result in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetInteger( arry: CGPDFArrayRef; index: size_t; var value: CGPDFInteger ): CBool; external name '_CGPDFArrayGetInteger'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a number (real or
 * integer), return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFArrayGetNumber( arry: CGPDFArrayRef; index: size_t; var value: CGPDFReal ): CBool; external name '_CGPDFArrayGetNumber'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a name, return the
 * result in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetName( arry: CGPDFArrayRef; index: size_t; var value: ConstCStringPtr ): CBool; external name '_CGPDFArrayGetName'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a string, return
 * the result in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetString( arry: CGPDFArrayRef; index: size_t; var value: CGPDFStringRef ): CBool; external name '_CGPDFArrayGetString'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's an array, return
 * it in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetArray( arry: CGPDFArrayRef; index: size_t; var value: CGPDFArrayRef ): CBool; external name '_CGPDFArrayGetArray'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a dictionary,
 * return it in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetDictionary( arry: CGPDFArrayRef; index: size_t; var value: CGPDFDictionaryRef ): CBool; external name '_CGPDFArrayGetDictionary'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object at `index' in `array' and, if it's a stream, return
 * it in `value'.  Return true on success; false otherwise. }

function CGPDFArrayGetStream( arry: CGPDFArrayRef; index: size_t; var value: CGPDFStreamRef ): CBool; external name '_CGPDFArrayGetStream'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.

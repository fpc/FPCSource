{ CoreGraphics - CGPDFDictionary.h
 * Copyright (c) 2002-2004 Apple Computer, Inc.
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

unit CGPDFDictionary;
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
uses MacTypes,CGPDFObject,CGBase;
{$ALIGN POWER}


// CGPDFDictionaryRef defined in CGBase


{ Return the number of entries in `dictionary'. }

function CGPDFDictionaryGetCount( dict: CGPDFDictionaryRef ): size_t; external name '_CGPDFDictionaryGetCount'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and return the result
 * in `value'. Return true on success; false otherwise. }

function CGPDFDictionaryGetObject( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFObjectRef ): CBool; external name '_CGPDFDictionaryGetObject'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a
 * boolean, return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetBoolean( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFBoolean ): CBool; external name '_CGPDFDictionaryGetBoolean'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's an
 * integer, return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetInteger( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFInteger ): CBool; external name '_CGPDFDictionaryGetInteger'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a number
 * (real or integer), return the result in `value'.  Return true on
 * success; false otherwise. }

function CGPDFDictionaryGetNumber( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFReal ): CBool; external name '_CGPDFDictionaryGetNumber'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a name,
 * return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetName( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: ConstCStringPtr ): CBool; external name '_CGPDFDictionaryGetName'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a
 * string, return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetString( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFStringRef ): CBool; external name '_CGPDFDictionaryGetString'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's an
 * array, return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetArray( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFArrayRef ): CBool; external name '_CGPDFDictionaryGetArray'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a
 * dictionary, return the result in `value'.  Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetDictionary( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFDictionaryRef ): CBool; external name '_CGPDFDictionaryGetDictionary'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Look up the object associated with `key' in `dict' and, if it's a
 * stream, return the result in `value'. Return true on success; false
 * otherwise. }

function CGPDFDictionaryGetStream( dict: CGPDFDictionaryRef; key: ConstCStringPtr; var value: CGPDFStreamRef ): CBool; external name '_CGPDFDictionaryGetStream'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ The callback for `CGPDFDictionaryApplyFunction'.  `key' is the current
 * key, `value' is the value for `key', and `info' is the parameter passed
 * to `CGPDFDictionaryApplyFunction'. }

type
	CGPDFDictionaryApplierFunction = procedure( key: ConstCStringPtr; value: CGPDFObjectRef; info: UnivPtr );

{ Enumerate all of the keys in `dict', calling `function' once for each
 * key/value pair.  Passes the current key, the associated value, and
 * `info' to `function'. }

procedure CGPDFDictionaryApplyFunction( dict: CGPDFDictionaryRef; func: CGPDFDictionaryApplierFunction; info: UnivPtr ); external name '_CGPDFDictionaryApplyFunction'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.

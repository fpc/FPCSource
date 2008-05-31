{ CoreGraphics - CGPDFScanner.h
 * Copyright (c) 2004 Apple Computer, Inc.
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

unit CGPDFScanner;
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
uses MacTypes,CGBase,CGPDFObject;
{$ALIGN POWER}


// CGPDFScannerRef defined in CGBase


{ Create a scanner. }

function CGPDFScannerCreate( cs: CGPDFContentStreamRef; table: CGPDFOperatorTableRef; info: UnivPtr ): CGPDFScannerRef; external name '_CGPDFScannerCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Retain `scanner'. }

function CGPDFScannerRetain( scanner: CGPDFScannerRef ): CGPDFScannerRef; external name '_CGPDFScannerRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Release `scanner'. }

procedure CGPDFScannerRelease( scanner: CGPDFScannerRef ); external name '_CGPDFScannerRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Scan the content stream of `scanner'. Returns true if the entire stream
 * was scanned successfully; false if scanning failed for some reason (for
 * example, if the stream's data is corrupted). }

function CGPDFScannerScan( scanner: CGPDFScannerRef ): CBool; external name '_CGPDFScannerScan'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the content stream associated with `scanner'. }

function CGPDFScannerGetContentStream( scanner: CGPDFScannerRef ): CGPDFContentStreamRef; external name '_CGPDFScannerGetContentStream'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and return it in `value'. }

function CGPDFScannerPopObject( scanner: CGPDFScannerRef; var value: CGPDFObjectRef ): CBool; external name '_CGPDFScannerPopObject'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a boolean, return
 * it in `value'. Return false if the top of the stack isn't a boolean. }

function CGPDFScannerPopBoolean( scanner: CGPDFScannerRef; var value: CGPDFBoolean ): CBool; external name '_CGPDFScannerPopBoolean'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's an integer,
 * return it in `value'. Return false if the top of the stack isn't an
 * integer. }

function CGPDFScannerPopInteger( scanner: CGPDFScannerRef; var value: CGPDFInteger ): CBool; external name '_CGPDFScannerPopInteger'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a number, return
 * it in `value'. Return false if the top of the stack isn't a number. }

function CGPDFScannerPopNumber( scanner: CGPDFScannerRef; var value: CGPDFReal ): CBool; external name '_CGPDFScannerPopNumber'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a name, return it
 * in `value'. Return false if the top of the stack isn't a name. }

function CGPDFScannerPopName( scanner: CGPDFScannerRef; var value: ConstCStringPtr ): CBool; external name '_CGPDFScannerPopName'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a string, return
 * it in `value'. Return false if the top of the stack isn't a string. }

function CGPDFScannerPopString( scanner: CGPDFScannerRef; var value: CGPDFStringRef ): CBool; external name '_CGPDFScannerPopString'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's an array, return
 * it in `value'. Return false if the top of the stack isn't an array. }

function CGPDFScannerPopArray( scanner: CGPDFScannerRef; var value: CGPDFArrayRef ): CBool; external name '_CGPDFScannerPopArray'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a dictionary,
 * return it in `value'. Return false if the top of the stack isn't a
 * dictionary. }

function CGPDFScannerPopDictionary( scanner: CGPDFScannerRef; var value: CGPDFDictionaryRef ): CBool; external name '_CGPDFScannerPopDictionary'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Pop an object from the stack of `scanner' and, if it's a stream, return
 * it in `value'. Return false if the top of the stack isn't a stream. }

function CGPDFScannerPopStream( scanner: CGPDFScannerRef; var value: CGPDFStreamRef ): CBool; external name '_CGPDFScannerPopStream'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.

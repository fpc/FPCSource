{ CoreGraphics - CGPDFDocument.h
 * Copyright (c) 2000-2003 Apple Computer, Inc.
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

unit CGPDFDocument;
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
uses MacTypes,CFBase,CGBase,CGDataProvider,CGGeometry,CFURL;
{$ALIGN POWER}


type
	CGPDFDocumentRef = ^SInt32; { an opaque 32-bit type }


{ Create a PDF document, using `provider' to obtain the document's
 * data. }

function CGPDFDocumentCreateWithProvider( provider: CGDataProviderRef ): CGPDFDocumentRef; external name '_CGPDFDocumentCreateWithProvider';

{ Create a PDF document from `url'. }

function CGPDFDocumentCreateWithURL( url: CFURLRef ): CGPDFDocumentRef; external name '_CGPDFDocumentCreateWithURL';

{ Equivalent to `CFRetain(document)', except it doesn't crash (as CFRetain
 * does) if `document' is NULL. }

function CGPDFDocumentRetain( document: CGPDFDocumentRef ): CGPDFDocumentRef; external name '_CGPDFDocumentRetain';

{ Equivalent to `CFRelease(document)', except it doesn't crash (as
 * CFRelease does) if `document' is NULL. }

procedure CGPDFDocumentRelease( document: CGPDFDocumentRef ); external name '_CGPDFDocumentRelease';

{ Return the major and minor version numbers of `document'. }

procedure CGPDFDocumentGetVersion( document: CGPDFDocumentRef; var majorVersion: SInt32; var minorVersion: SInt32 ); external name '_CGPDFDocumentGetVersion'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return true if the PDF file associated with `document' is encrypted;
 * false otherwise.  If the PDF file is encrypted, then a password must be
 * supplied before certain operations are enabled; different passwords may
 * enable different operations. }

function CGPDFDocumentIsEncrypted( document: CGPDFDocumentRef ): CBool; external name '_CGPDFDocumentIsEncrypted'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Use `password' to decrypt `document' and grant permission for certain
 * operations.  Returns true if `password' is a valid password; false
 * otherwise. }

function CGPDFDocumentUnlockWithPassword( document: CGPDFDocumentRef; password: ConstCStringPtr ): CBool; external name '_CGPDFDocumentUnlockWithPassword'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `document' is unlocked; false otherwise.  A document is
 * unlocked if it isn't encrypted, or if it is encrypted and a valid password
 * was previously specified with CGPDFDocumentUnlockWithPassword. }

function CGPDFDocumentIsUnlocked( document: CGPDFDocumentRef ): CBool; external name '_CGPDFDocumentIsUnlocked'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `document' allows printing; false otherwise.  Typically,
 * this function returns false only if the document is encrypted and the
 * document's current password doesn't grant permission to perform
 * printing. }

function CGPDFDocumentAllowsPrinting( document: CGPDFDocumentRef ): CBool; external name '_CGPDFDocumentAllowsPrinting'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `document' allows copying; false otherwise.  Typically,
 * this function returns false only if the document is encrypted and the
 * document's current password doesn't grant permission to perform
 * copying. }

function CGPDFDocumentAllowsCopying( document: CGPDFDocumentRef ): CBool; external name '_CGPDFDocumentAllowsCopying'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the number of pages in `document'. }

function CGPDFDocumentGetNumberOfPages( document: CGPDFDocumentRef ): size_t; external name '_CGPDFDocumentGetNumberOfPages';

{ Return the page corresponding to `pageNumber', or NULL if no such page
 * exists in the document.  Pages are numbered starting at 1. }

function CGPDFDocumentGetPage( document: CGPDFDocumentRef; pageNumber: size_t ): CGPDFPageRef; external name '_CGPDFDocumentGetPage'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the document catalog of `document'. }

function CGPDFDocumentGetCatalog( document: CGPDFDocumentRef ): CGPDFDictionaryRef; external name '_CGPDFDocumentGetCatalog'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the info dictionary of `document'. }

function CGPDFDocumentGetInfo( document: CGPDFDocumentRef ): CGPDFDictionaryRef; external name '_CGPDFDocumentGetInfo'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the "file identifier" of `document'. }

function CGPDFDocumentGetID( document: CGPDFDocumentRef ): CGPDFArrayRef; external name '_CGPDFDocumentGetID'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the CFTypeID for CGPDFDocumentRefs. }

function CGPDFDocumentGetTypeID: CFTypeID; external name '_CGPDFDocumentGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ The following functions are deprecated in favor of the CGPDFPage API. }

{ DEPRECATED; return the media box of page number `page' in `document'. }

function CGPDFDocumentGetMediaBox( document: CGPDFDocumentRef; page: SInt32 ): CGRect; external name '_CGPDFDocumentGetMediaBox';

{ DEPRECATED; return the crop box of page number `page' in `document'. }

function CGPDFDocumentGetCropBox( document: CGPDFDocumentRef; page: SInt32 ): CGRect; external name '_CGPDFDocumentGetCropBox';

{ DEPRECATED; return the bleed box of page number `page' in `document'. }

function CGPDFDocumentGetBleedBox( document: CGPDFDocumentRef; page: SInt32 ): CGRect; external name '_CGPDFDocumentGetBleedBox';

{ DEPRECATED; return the trim box of page number `page' in `document'. }

function CGPDFDocumentGetTrimBox( document: CGPDFDocumentRef; page: SInt32 ): CGRect; external name '_CGPDFDocumentGetTrimBox';

{ DEPRECATED; return the art box of page number `page' in `document'. }

function CGPDFDocumentGetArtBox( document: CGPDFDocumentRef; page: SInt32 ): CGRect; external name '_CGPDFDocumentGetArtBox';

{ DEPRECATED; return the rotation angle (in degrees) of page number `page'
 * in `document'. }

function CGPDFDocumentGetRotationAngle( document: CGPDFDocumentRef; page: SInt32 ): SInt32; external name '_CGPDFDocumentGetRotationAngle';


end.

{
     File:       QD/QDPictToCGContext.h
 
     Contains:   API to draw Quickdraw PICTs into CoreGraphics context
 
     Version:    Quickdraw-150~1
 
     Copyright:  © 2001-2003 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit QDPictToCGContext;
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
uses MacTypes,CGContext,CGGeometry,CGDataProvider,CFURL;
{$ALIGN MAC68K}

type
	QDPictRef    = ^SInt32; { an opaque 32-bit type }

{
    Note: QuickDraw picture data typically comes in two forms: a PICT resource
    that begins the picture header data at the beginning of the resource and PICT
    files that begin with 512 bytes of arbitrary data, followed by
    the picture header data. For this reason, the routines that create a QDPictRef
    attempt to find the picture header data beginning at either the first byte
    of the data provided or at byte 513 of the data provided.
    
    Additionally the Picture Bounds must not be an empty rect.
}
{ Create a QDPict reference, using `provider' to obtain the QDPict's data. 
 * It is assumed that either the first byte or the 513th byte of data
 * in the file referenced by the URL is the first byte of the
 * picture header. If the URL does not begin PICT data at one
 * of these places in the data fork then the QDPictRef returned will be NULL.
}
{
 *  QDPictCreateWithProvider()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function QDPictCreateWithProvider( provider: CGDataProviderRef ): QDPictRef; external name '_QDPictCreateWithProvider';


{ Create a QDPict reference from `url'. 
 * It is assumed that either the first byte or the 513th byte of data
 * in the file referenced by the URL is the first byte of the
 * picture header. If the URL does not begin PICT data at one
 * of these places in the data fork then the QDPictRef returned will be NULL.
}
{
 *  QDPictCreateWithURL()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function QDPictCreateWithURL( url: CFURLRef ): QDPictRef; external name '_QDPictCreateWithURL';


{ Increment the retain count of `pictRef' and return it.  All 
 * pictRefs are created with an initial retain count of 1. }
{
 *  QDPictRetain()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function QDPictRetain( pictRef: QDPictRef ): QDPictRef; external name '_QDPictRetain';


{ Decrement the retain count of `pictRef'.  If the retain count reaches 0,
 * then free it and any associated resources. }
{
 *  QDPictRelease()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure QDPictRelease( pictRef: QDPictRef ); external name '_QDPictRelease';


{ Return the Picture Bounds of the QuickDraw picture represented by `pictRef'. This
    rectangle is in the default user space with one unit = 1/72 inch.
}
{
 *  QDPictGetBounds()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function QDPictGetBounds( pictRef: QDPictRef ): CGRect; external name '_QDPictGetBounds';


{ Return the resolution of the QuickDraw picture represented by `pictRef'.
    This data, together with the CGRect returned by QDPictGetBounds, can be
    used to compute the size of the picture in pixels, which is what QuickDraw
    really records into pictures.
}
{
 *  QDPictGetResolution()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
procedure QDPictGetResolution( pictRef: QDPictRef; var xRes, yRes: Float32 ); external name '_QDPictGetResolution';


{ Draw `pictRef' in the rectangular area specified by `rect'.
 * The PICT bounds of the page is scaled, if necessary, to fit into
 * `rect'. To get unscaled results, supply a rect the size of the rect
 * returned by QDPictGetBounds.
}
{
 *  QDPictDrawToCGContext()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
// AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER
function QDPictDrawToCGContext( ctx: CGContextRef; rect: CGRect; pictRef: QDPictRef ): OSStatus; external name '_QDPictDrawToCGContext';

end.

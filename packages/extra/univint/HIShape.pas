{
     File:       HIToolbox/HIShape.h
 
     Contains:   Generic Abstract Shape API
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 2001-2005 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{     File:       HIShape.p(.pas)																		}
{ 																										}
{     Contains:   CodeWarrior Pascal(GPC) translation of Apple's Mac OS X 10.2 introduced HIShape.h.	}
{				  Translation compatible with make-gpc-interfaces.pl generated MWPInterfaces            }
{                 (GPCPInterfaces) and Mac OS X 10.2.x or higher.  The CodeWarrior Pascal translation   }
{                 is linkable with Mac OS X 10.2.x or higher CFM CarbonLib.  The GPC translation is     }
{                 linkable with Mac OS X 10.2.x or higher Mach-O Carbon.framework.                      }
{ 																										}
{     Version:    1.0																					}
{ 																										}
{	  Pascal Translation:  Gale Paeper, <gpaeper@empirenet.com>, 2004									}
{ 																										}
{     Copyright:  Subject to the constraints of Apple's original rights, you're free to use this		}
{				  translation as you deem fit.															}
{ 																										}
{     Bugs?:      This is an AS IS translation with no express guarentees of any kind.					}
{                 If you do find a bug, please help out the Macintosh Pascal programming community by   }
{				  reporting your bug finding and possible fix to either personal e-mail to Gale Paeper	}
{				  or a posting to the MacPascal mailing list.											}
{                                                                                                       }
{     Translation assisted by:                                                                          }
{This file was processed by Dan's Source Converter}
{version 1.3 (this version modified by Ingemar Ragnemalm)}
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit HIShape;
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
uses MacTypes,CFBase,CGContext,Drag,Quickdraw,CarbonEvents,HIGeometry;
{$ALIGN POWER}


{
 *  HIShape
 *  
 *  Discussion:
 *    HIShape is an abstract shape object for use with some of the
 *    HIToolbox APIs. It is designed as a replacement for RgnHandles
 *    (though it is currently implemented in terms of them at the time
 *    of this writing). This abstraction will allow us to avoid using
 *    QD types in our APIs, particularly in HIView. 
 *    
 *    One of the biggest benefits of HIShape is that we have mutable
 *    and immutable variants. This means that immutable shapes can be
 *    created and passed around and 'copied' very quickly, since they
 *    are actually refcounted when copied. This avoids needing to do
 *    the handle-to-handle copies that occur right now with
 *    RgnHandle-based APIs.
 }
type
	HIShapeRef = ^SInt32; { an opaque 32-bit type }
type
	HIMutableShapeRef = ^SInt32; { an opaque 32-bit type }


{
 *  HIShapeGetTypeID()
 *  
 *  Discussion:
 *    Returns the CF type ID for the HIShape class.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    A CF type ID.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeGetTypeID: CFTypeID; external name '_HIShapeGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{======================================================================================}
{  IMMUTABLE FUNCTIONS                                                                 }
{======================================================================================}
{
 *  HIShapeCreateEmpty()
 *  
 *  Discussion:
 *    Creates an immutable empty shape. Useful at times.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    An immutable, empty HIShape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateEmpty: HIShapeRef; external name '_HIShapeCreateEmpty';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIShapeCreateWithQDRgn()
 *  
 *  Discussion:
 *    Creates an immutable shape based on an existing Quickdraw region
 *    handle.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inRgn:
 *      The Quickdraw region from which to create the HIShape.
 *  
 *  Result:
 *    An immutable HIShape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateWithQDRgn( inRgn: RgnHandle ): HIShapeRef; external name '_HIShapeCreateWithQDRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateWithRect()
 *  
 *  Discussion:
 *    Creates an immutable, rectangular shape based on a given
 *    rectangle.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inRect:
 *      The HIRect from which to create the resulting shape.
 *  
 *  Result:
 *    An immutable HIShape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateWithRect( const (*var*) inRect: HIRect ): HIShapeRef; external name '_HIShapeCreateWithRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateCopy()
 *  
 *  Discussion:
 *    Creates an immutable copy of a mutable or immutable HIShape.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      The existing HIShapeRef you wish to copy.
 *  
 *  Result:
 *    An immutable HIShape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateCopy( inShape: HIShapeRef ): HIShapeRef; external name '_HIShapeCreateCopy';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateIntersection()
 *  
 *  Discussion:
 *    Creates a new immutable shape which is the intersection of two
 *    others.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      An existing HIShapeRef.
 *    
 *    inShape2:
 *      An existing HIShapeRef.
 *  
 *  Result:
 *    A new immutable HIShapeRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateIntersection( inShape1: HIShapeRef; inShape2: HIShapeRef ): HIShapeRef; external name '_HIShapeCreateIntersection';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateDifference()
 *  
 *  Discussion:
 *    Creates a new immutable shape which is the difference of two
 *    others. The second shape is subtracted from the first.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      An existing HIShapeRef.
 *    
 *    inShape2:
 *      An existing HIShapeRef.
 *  
 *  Result:
 *    A new immutable HIShapeRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateDifference( inShape1: HIShapeRef; inShape2: HIShapeRef ): HIShapeRef; external name '_HIShapeCreateDifference';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateUnion()
 *  
 *  Discussion:
 *    Creates a new immutable shape which is the union of two others.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      An existing HIShapeRef.
 *    
 *    inShape2:
 *      An existing HIShapeRef.
 *  
 *  Result:
 *    A new immutable HIShapeRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateUnion( inShape1: HIShapeRef; inShape2: HIShapeRef ): HIShapeRef; external name '_HIShapeCreateUnion';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeIsEmpty()
 *  
 *  Discussion:
 *    Returns true if the given HIShapeRef is empty, i.e. its area is
 *    empty.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      The existing HIShapeRef you wish to test.
 *  
 *  Result:
 *    A boolean result.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeIsEmpty( inShape: HIShapeRef ): Boolean; external name '_HIShapeIsEmpty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeIsRectangular()
 *  
 *  Discussion:
 *    Returns true if the given HIShapeRef is rectangular.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      The existing HIShapeRef you wish to test.
 *  
 *  Result:
 *    A boolean result.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeIsRectangular( inShape: HIShapeRef ): Boolean; external name '_HIShapeIsRectangular';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeContainsPoint()
 *  
 *  Discussion:
 *    Returns true if the given HIShapeRef contains the point passed in.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    inPoint:
 *      The point to check.
 *  
 *  Result:
 *    A boolean result.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeContainsPoint( inShape: HIShapeRef; const (*var*) inPoint: HIPoint ): Boolean; external name '_HIShapeContainsPoint';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeIntersectsRect()
 *  
 *  Discussion:
 *    Returns true if the given HIShapeRef intersects the rect passed
 *    in.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    inRect:
 *      The rectangle to check.
 *  
 *  Result:
 *    A boolean result.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeIntersectsRect( inShape: HIShapeRef; const (*var*) inRect: HIRect ): Boolean; external name '_HIShapeIntersectsRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIShapeGetBounds()
 *  
 *  Discussion:
 *    Returns the bounding rectangle of a given HIShapeRef.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    outRect:
 *      Receives the bounding rectangle.
 *  
 *  Result:
 *    A pointer to the rectangle you passed in, for convenience.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeGetBounds( inShape: HIShapeRef; var outRect: HIRect ): HIRectPtr; external name '_HIShapeGetBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeGetAsQDRgn()
 *  
 *  Discussion:
 *    Changes a given Quickdraw region handle to have the same shape as
 *    a given HIShapeRef. Essentially you are converting an HIShapeRef
 *    into a RgnHandle. This conversion may lose fidelity depending on
 *    how the shape was created originally.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    outRgn:
 *      An existing region to change.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeGetAsQDRgn( inShape: HIShapeRef; outRgn: RgnHandle ): OSStatus; external name '_HIShapeGetAsQDRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeReplacePathInCGContext()
 *  
 *  Discussion:
 *    Given an HIShapeRef and a CGContextRef, make the current path in
 *    the context represent the shape. You might use this to clip to a
 *    shape, for example. You could call this function and then
 *    immediately call CGContextClip.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    inContext:
 *      The context to apply the shape to.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeReplacePathInCGContext( inShape: HIShapeRef; inContext: CGContextRef ): OSStatus; external name '_HIShapeReplacePathInCGContext';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeSetQDClip()
 *  
 *  Discussion:
 *    Given an HIShapeRef and a Quickdraw port, set the current clip in
 *    the port to the shape.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      An existing HIShapeRef.
 *    
 *    inPort:
 *      The port to set the clip for.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeSetQDClip( inShape: HIShapeRef; inPort: CGrafPtr ): OSStatus; external name '_HIShapeSetQDClip';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{======================================================================================}
{  MUTABLE FUNCTIONS                                                                   }
{======================================================================================}
{
 *  HIShapeCreateMutable()
 *  
 *  Discussion:
 *    Creates a new, mutable, empty shape.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    A mutable shape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateMutable: HIMutableShapeRef; external name '_HIShapeCreateMutable';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeCreateMutableCopy()
 *  
 *  Discussion:
 *    Given an existing HIShapeRef, creates a new mutable copy.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inOrig:
 *      The shape to copy.
 *  
 *  Result:
 *    A mutable shape reference.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeCreateMutableCopy( inOrig: HIShapeRef ): HIMutableShapeRef; external name '_HIShapeCreateMutableCopy';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeSetEmpty()
 *  
 *  Discussion:
 *    Sets a mutable shape to be an empty shape.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      The shape to empty.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeSetEmpty( inShape: HIMutableShapeRef ): OSStatus; external name '_HIShapeSetEmpty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeIntersect()
 *  
 *  Discussion:
 *    Takes two shapes and sets a third to be their intersection.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      The first shape.
 *    
 *    inShape2:
 *      The second shape.
 *    
 *    outResult:
 *      The shape to receive the result of the intersection. This can
 *      be one of the source shapes.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeIntersect( inShape1: HIShapeRef; inShape2: HIShapeRef; outResult: HIMutableShapeRef ): OSStatus; external name '_HIShapeIntersect';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeDifference()
 *  
 *  Discussion:
 *    Takes two shapes and sets a third to be their difference. The
 *    second shape is subtracted from the first.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      The first shape.
 *    
 *    inShape2:
 *      The second shape.
 *    
 *    outResult:
 *      The shape to receive the result of the intersection. This can
 *      be one of the source shapes.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeDifference( inShape1: HIShapeRef; inShape2: HIShapeRef; outResult: HIMutableShapeRef ): OSStatus; external name '_HIShapeDifference';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeUnion()
 *  
 *  Discussion:
 *    Takes two shapes and sets a third to be their union.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape1:
 *      The first shape.
 *    
 *    inShape2:
 *      The second shape.
 *    
 *    outResult:
 *      The shape to receive the result of the union. This can be one
 *      of the source shapes.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeUnion( inShape1: HIShapeRef; inShape2: HIShapeRef; outResult: HIMutableShapeRef ): OSStatus; external name '_HIShapeUnion';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  HIShapeOffset()
 *  
 *  Discussion:
 *    Offsets a shape by some delta.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inShape:
 *      The shape to offset.
 *    
 *    inDX:
 *      The delta to move the shape on the X axis.
 *    
 *    inDY:
 *      The delta to move the shape on the Y axis.
 *  
 *  Result:
 *    An operating system status code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function HIShapeOffset( inShape: HIMutableShapeRef; inDX: Float32; inDY: Float32 ): OSStatus; external name '_HIShapeOffset';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


end.

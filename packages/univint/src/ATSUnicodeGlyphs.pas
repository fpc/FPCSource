{
     File:       QD/ATSUnicodeGlyphs.h
 
     Contains:   ATSUI glyph handling functions.
 
     Version:    Quickdraw-150~1
 
     Copyright:  © 2003 by Apple Computer, Inc., all rights reserved.
 
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

unit ATSUnicodeGlyphs;
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
uses MacTypes,ATSUnicodeTypes,TextCommon,ATSTypes;
{$ALIGN MAC68K}

{ ---------------------------------------------------------------------------- }
{ ATSUI glyph metrics                                                          }
{ ---------------------------------------------------------------------------- }


{
 *  ATSUGlyphGetIdealMetrics()
 *  
 *  Summary:
 *    Obtains resolution-independent font metric information for glyphs
 *    associated with a given style object.
 *  
 *  Discussion:
 *    The advance width is the full horizontal width of the glyph as
 *    measured from its origin to the origin of the next glyph on the
 *    line, including the left-side and right-side bearings. For
 *    vertical text, the advance height is the sum of the top-side
 *    bearing, the bounding-box height, and the bottom-side bearing.
 *    You can call the ATSUGlyphGetIdealMetrics function to obtain an
 *    array of ATSGlyphIdealMetrics structures containing values for
 *    the specified glyphs' advance and side bearings.
 *    ATSUGlyphGetIdealMetrics can analyze both horizontal and vertical
 *    text, automatically producing the appropriate bearing values
 *    (oriented for width or height, respectively) for each. You should
 *    call ATSUGlyphGetIdealMetrics to obtain resolution-independent
 *    glyph metrics. To obtain device-adjusted (that is,
 *    resolution-dependent) glyph metrics, call the function
 *    ATSUGlyphGetScreenMetrics.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referring to a font you wish to obtain glyph metrics
 *      from.
 *    
 *    iNumOfGlyphs:
 *      The number of glyph IDs you are passing in to be examined. This
 *      value should be equal to the size of the array you are passing
 *      in for the iGlyphIDs parameter.
 *    
 *    iGlyphIDs:
 *      An array of glyph IDs referring to glyphs for which you wish to
 *      obtain metrics.
 *    
 *    iInputOffset:
 *      A ByteOffset value specifying the offset in bytes between glyph
 *      IDs in the iGlyphIDs array.
 *    
 *    oIdealMetrics:
 *      A pointer to memory you have allocated for an array of
 *      ATSGlyphIdealMetrics structures. On return, each structure
 *      contains advance and side-bearing values for a glyph. See
 *      ATSTypes.h for more information regarding the
 *      ATSGlyphIdealMetrics structure.
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGlyphGetIdealMetrics( iATSUStyle: ATSUStyle; iNumOfGlyphs: ItemCount; iGlyphIDs: GlyphIDPtr; iInputOffset: ByteOffset; oIdealMetrics: ATSGlyphIdealMetricsPtr ): OSStatus; external name '_ATSUGlyphGetIdealMetrics';


{
 *  ATSUGlyphGetScreenMetrics()
 *  
 *  Summary:
 *    Obtains device-adjusted font metric information for glyphs
 *    associated with a given style object.
 *  
 *  Discussion:
 *    You can call the ATSUGlyphGetScreenMetrics function to obtain an
 *    array of ATSGlyphScreenMetrics structures containing values for
 *    the specified glyphs' advance and side bearings, top left,
 *    height, and width. You should call ATSUGlyphGetScreenMetrics to
 *    obtain device-adjusted (that is, resolution-dependent) glyph
 *    metrics. To obtain resolution-independent glyph metrics, call the
 *    function ATSUGlyphGetIdealMetrics.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referring to a font you wish to obtain glyph metrics
 *      from.
 *    
 *    iNumOfGlyphs:
 *      The number of glyph IDs you are passing in to be examined. This
 *      value should be equal to the size of the array you are passing
 *      in for the iGlyphIDs parameter.
 *    
 *    iGlyphIDs:
 *      An array of glyph IDs referring to glyphs for which you wish to
 *      obtain metrics.
 *    
 *    iInputOffset:
 *      A ByteOffset value specifying the offset in bytes between glyph
 *      IDs in the iGlyphIDs array.
 *    
 *    iForcingAntiAlias:
 *      A Boolean value indicating whether anti-aliasing is forced for
 *      the style object.
 *    
 *    iAntiAliasSwitch:
 *      A Boolean value indicating whether anti-aliasing is currently
 *      on or off.
 *    
 *    oScreenMetrics:
 *      A pointer to memory you have allocated for an array of
 *      ATSGlyphScreenMetrics structures. On return, each structure
 *      contains advance and side-bearing values for a glyph. See
 *      ATSTypes.h for more information regarding the
 *      ATSGlyphScreenMetrics structure.
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGlyphGetScreenMetrics( iATSUStyle: ATSUStyle; iNumOfGlyphs: ItemCount; iGlyphIDs: GlyphIDPtr; iInputOffset: ByteOffset; iForcingAntiAlias: Boolean; iAntiAliasSwitch: Boolean; oScreenMetrics: ATSGlyphScreenMetricsPtr ): OSStatus; external name '_ATSUGlyphGetScreenMetrics';


{ ---------------------------------------------------------------------------- }
{ ATSUI glyph curve access functions and callbacks                             }
{ ---------------------------------------------------------------------------- }
{
 *  ATSUGetNativeCurveType()
 *  
 *  Summary:
 *    Returns the native curve format for a specific font.
 *  
 *  Discussion:
 *    Use this function to decide whether to call
 *    ATSUGlyphGetQuadraticPaths or ATSUGlyphGetCubicPaths. Both
 *    functions will return curves for all valid ATSUI fonts, but if
 *    the curve type you request is not the native curve type of the
 *    font, the curves you get back will be mathematically converted,
 *    rather than native font data. See the definition of ATSCurveType
 *    in ATSTypes.h for possible return values from this function.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referencing a font for which you wish to obtain the
 *      native curve type.
 *    
 *    oCurveType:
 *      On return, a value indicating the native curve type of the font
 *      referenced by iATSUStyle. See the definition of ATSCurveType in
 *      ATSTypes.h for a list of possible return values for this
 *      parameter.
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGetNativeCurveType( iATSUStyle: ATSUStyle; var oCurveType: ATSCurveType ): OSStatus; external name '_ATSUGetNativeCurveType';


{
 *  ATSQuadraticNewPathProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to start a new drawing path.
 *  
 *  Parameters:
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetQuadraticPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetQuadraticPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSQuadraticNewPathProcPtr = function( callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSQuadraticNewPathProcPtr)              ATSQuadraticNewPathUPP;
type ATSQuadraticNewPathUPP = Ptr;

{
 *  NewATSQuadraticNewPathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSQuadraticNewPathUPP( userRoutine: ATSQuadraticNewPathProcPtr ): ATSQuadraticNewPathUPP; external name '_NewATSQuadraticNewPathUPP';

{
 *  DisposeATSQuadraticNewPathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSQuadraticNewPathUPP( userUPP: ATSQuadraticNewPathUPP ); external name '_DisposeATSQuadraticNewPathUPP';

{
 *  InvokeATSQuadraticNewPathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSQuadraticNewPathUPP( callBackDataPtr: UnivPtr; userUPP: ATSQuadraticNewPathUPP ): OSStatus; external name '_InvokeATSQuadraticNewPathUPP';


{
 *  ATSQuadraticLineProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to draw straight lines.
 *  
 *  Parameters:
 *    
 *    pt1:
 *      The starting point of the line.
 *    
 *    pt2:
 *      The end point of the line.
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetQuadraticPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetQuadraticPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSQuadraticLineProcPtr = function( const (*var*) pt1, pt2: Float32Point; callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSQuadraticLineProcPtr)                 ATSQuadraticLineUPP;
type ATSQuadraticLineUPP = Ptr;
{
 *  NewATSQuadraticLineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSQuadraticLineUPP( userRoutine: ATSQuadraticLineProcPtr ): ATSQuadraticLineUPP; external name '_NewATSQuadraticLineUPP';

{
 *  DisposeATSQuadraticLineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSQuadraticLineUPP( userUPP: ATSQuadraticLineUPP ); external name '_DisposeATSQuadraticLineUPP';

{
 *  InvokeATSQuadraticLineUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSQuadraticLineUPP( const (*var*) pt1, pt2: Float32Point; callBackDataPtr: UnivPtr; userUPP: ATSQuadraticLineUPP ): OSStatus; external name '_InvokeATSQuadraticLineUPP';


{
 *  ATSQuadraticCurveProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to draw curves. The curve is a quadratic patch specified by a
 *    start point (pt1), and end point (pt2), and a single control
 *    point (controlPt).
 *  
 *  Parameters:
 *    
 *    pt1:
 *      The starting point of the curve.
 *    
 *    controlPt:
 *      The off-curve control point.
 *    
 *    pt2:
 *      The end point of the curve.
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetQuadraticPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetQuadraticPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSQuadraticCurveProcPtr = function( const (*var*) pt1, controlPt, pt2: Float32Point; callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSQuadraticCurveProcPtr)                ATSQuadraticCurveUPP;
type ATSQuadraticCurveUPP = Ptr;
{
 *  NewATSQuadraticCurveUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSQuadraticCurveUPP( userRoutine: ATSQuadraticCurveProcPtr ): ATSQuadraticCurveUPP; external name '_NewATSQuadraticCurveUPP';

{
 *  DisposeATSQuadraticCurveUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSQuadraticCurveUPP( userUPP: ATSQuadraticCurveUPP ); external name '_DisposeATSQuadraticCurveUPP';

{
 *  InvokeATSQuadraticCurveUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSQuadraticCurveUPP( const (*var*) pt1, controlPt, pt2: Float32Point; callBackDataPtr: UnivPtr; userUPP: ATSQuadraticCurveUPP ): OSStatus; external name '_InvokeATSQuadraticCurveUPP';


{
 *  ATSQuadraticClosePathProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to close the current drawing path.
 *  
 *  Parameters:
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetQuadraticPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetQuadraticPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSQuadraticClosePathProcPtr = function( callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSQuadraticClosePathProcPtr)            ATSQuadraticClosePathUPP;
type ATSQuadraticClosePathUPP = Ptr;
{
 *  NewATSQuadraticClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSQuadraticClosePathUPP( userRoutine: ATSQuadraticClosePathProcPtr ): ATSQuadraticClosePathUPP; external name '_NewATSQuadraticClosePathUPP';

{
 *  DisposeATSQuadraticClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSQuadraticClosePathUPP( userUPP: ATSQuadraticClosePathUPP ); external name '_DisposeATSQuadraticClosePathUPP';

{
 *  InvokeATSQuadraticClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSQuadraticClosePathUPP( callBackDataPtr: UnivPtr; userUPP: ATSQuadraticClosePathUPP ): OSStatus; external name '_InvokeATSQuadraticClosePathUPP';

{
 *  ATSUGlyphGetQuadraticPaths()
 *  
 *  Summary:
 *    Uses a callback mechanism to obtain a set of Quadratic outlines
 *    for a specified glyph in a specified font.
 *  
 *  Discussion:
 *    This function will allow you to use callbacks to obtain the exact
 *    outline of a specified glyph, in quadratic form. Although this
 *    function will always return results for any valid ATSUI font, you
 *    should first use the function ATSUGetNativeCurveType to determine
 *    the native format of the glyph you are interested in. Then,
 *    either call ATSUGlyphGetQuadraticPaths or ATSUGlyphGetCubicPaths
 *    based on the result. Otherwise, you may end up with curves that
 *    are mathematically converted from cubic to quadratic (or vice
 *    versa), instead of getting native font data. See the definitions
 *    of ATSQuadraticNewPathProcPtr, ATSQuadraticLineProcPtr,
 *    ATSQuadraticCurveProcPtr, and ATSQuadraticClosePathProcPtr for
 *    more information about setting up the callbacks.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referring to a font you wish to obtain a set of glyph
 *      outlines from.
 *    
 *    iGlyphID:
 *      A ID number referring to the glyph for which you wish to obtain
 *      outline data. Use the ATSUI direct access functions in
 *      ATSUnicodeDirectAccess.h to obtain values to pass for this
 *      parameter.
 *    
 *    iNewPathProc:
 *      A pointer to a callback function for quadratic new path
 *      operations. See the definition of ATSQuadraticNewPathProcPtr
 *      for more information about creating, disposing, and invoking
 *      this type of Universal Procedure Pointer.
 *    
 *    iLineProc:
 *      A pointer to a callback function for quadratic LineTo
 *      operations. See the definition of ATSQuadraticLineProcPtr for
 *      more information about creating, disposing, and invoking this
 *      type of Universal Procedure Pointer.
 *    
 *    iCurveProc:
 *      A pointer to a callback function for quadratic curve
 *      operations. See the definition of ATSQuadraticCurveProcPtr for
 *      more information about creating, disposing, and invoking this
 *      type of Universal Procedure Pointer.
 *    
 *    iClosePathProc:
 *      A pointer to a callback function for quadratic close path
 *      operations. See the definition of ATSQuadraticClosePathProcPtr
 *      for more information about creating, disposing, and invoking
 *      this type of Universal Procedure Pointer.
 *    
 *    iCallbackDataPtr:
 *      Any valid pointer. Any application specific data you wish to
 *      pass to your callbacks may be sent through this parameter.
 *    
 *    oCallbackResult:
 *      On return, status returned by callback functions. If an error
 *      occurs, callbacks may communicate it through this parameter.
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGlyphGetQuadraticPaths( iATSUStyle: ATSUStyle; iGlyphID: GlyphID; iNewPathProc: ATSQuadraticNewPathUPP; iLineProc: ATSQuadraticLineUPP; iCurveProc: ATSQuadraticCurveUPP; iClosePathProc: ATSQuadraticClosePathUPP; iCallbackDataPtr: UnivPtr; var oCallbackResult: OSStatus ): OSStatus; external name '_ATSUGlyphGetQuadraticPaths';


{
 *  ATSCubicMoveToProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to move the current pen location.
 *  
 *  Parameters:
 *    
 *    pt:
 *      The point to which to move the current pen location.
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetCubicPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetCubicPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSCubicMoveToProcPtr = function( const (*var*) pt: Float32Point; callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSCubicMoveToProcPtr)                   ATSCubicMoveToUPP;
type ATSCubicMoveToUPP = Ptr;
{
 *  NewATSCubicMoveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSCubicMoveToUPP( userRoutine: ATSCubicMoveToProcPtr ): ATSCubicMoveToUPP; external name '_NewATSCubicMoveToUPP';

{
 *  DisposeATSCubicMoveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSCubicMoveToUPP( userUPP: ATSCubicMoveToUPP ); external name '_DisposeATSCubicMoveToUPP';

{
 *  InvokeATSCubicMoveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSCubicMoveToUPP( const (*var*) pt: Float32Point; callBackDataPtr: UnivPtr; userUPP: ATSCubicMoveToUPP ): OSStatus; external name '_InvokeATSCubicMoveToUPP';


{
 *  ATSCubicLineToProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to draw straight lines.
 *  
 *  Parameters:
 *    
 *    pt:
 *      The end point of the line to be drawn. The starting point is
 *      whatever the current pen position is.
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetCubicPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetCubicPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSCubicLineToProcPtr = function( const (*var*) pt: Float32Point; callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSCubicLineToProcPtr)                   ATSCubicLineToUPP;
type ATSCubicLineToUPP = Ptr;
{
 *  NewATSCubicLineToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSCubicLineToUPP( userRoutine: ATSCubicLineToProcPtr ): ATSCubicLineToUPP; external name '_NewATSCubicLineToUPP';

{
 *  DisposeATSCubicLineToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSCubicLineToUPP( userUPP: ATSCubicLineToUPP ); external name '_DisposeATSCubicLineToUPP';

{
 *  InvokeATSCubicLineToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSCubicLineToUPP( const (*var*) pt: Float32Point; callBackDataPtr: UnivPtr; userUPP: ATSCubicLineToUPP ): OSStatus; external name '_InvokeATSCubicLineToUPP';


{
 *  ATSCubicCurveToProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to draw a curve. The curve is a Bezier patch defined by two
 *    off-curve control points (pt1 and pt2), and an endpoint (pt3).
 *    The starting point is whatever the current pen position is.
 *  
 *  Parameters:
 *    
 *    pt1:
 *      The first off-curve control point.
 *    
 *    pt2:
 *      The second off-curve control point.
 *    
 *    pt3:
 *      The end point of the curve.
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetCubicPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetCubicPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSCubicCurveToProcPtr = function( const (*var*) pt, pt2, pt3: Float32Point; callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSCubicCurveToProcPtr)                  ATSCubicCurveToUPP;
type ATSCubicCurveToUPP = Ptr;
{
 *  NewATSCubicCurveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSCubicCurveToUPP( userRoutine: ATSCubicCurveToProcPtr ): ATSCubicCurveToUPP; external name '_NewATSCubicCurveToUPP';

{
 *  DisposeATSCubicCurveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSCubicCurveToUPP( userUPP: ATSCubicCurveToUPP ); external name '_DisposeATSCubicCurveToUPP';

{
 *  InvokeATSCubicCurveToUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSCubicCurveToUPP( const (*var*) pt, pt2, pt3: Float32Point; callBackDataPtr: UnivPtr; userUPP: ATSCubicCurveToUPP ): OSStatus; external name '_InvokeATSCubicCurveToUPP';


{
 *  ATSCubicClosePathProcPtr
 *  
 *  Discussion:
 *    A pointer to a client supplied callback function for handling
 *    glyph curve drawing operations. This callback handles operations
 *    to close the current drawing path.
 *  
 *  Parameters:
 *    
 *    callBackDataPtr:
 *      A pointer to any application specific data that may have been
 *      passed to the callbacks through the iCallbackDataPtr parameter
 *      of the ATSUGlyphGetCubicPaths function.
 *  
 *  Result:
 *    Return status. Pass any errors you wish to propagate back to the
 *    original caller of ATSUGlyphGetCubicPaths through this return
 *    value. Note that any nonzero result from this callback will halt
 *    the curve drawing process.
 }
type ATSCubicClosePathProcPtr = function( callBackDataPtr: UnivPtr ): OSStatus;
// Beats me what this translates to.  If someone finds out they can tell me and we'll update it
// typedef STACK_UPP_TYPE(ATSCubicClosePathProcPtr)                ATSCubicClosePathUPP;
type ATSCubicClosePathUPP = Ptr;
{
 *  NewATSCubicClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function NewATSCubicClosePathUPP( userRoutine: ATSCubicClosePathProcPtr ): ATSCubicClosePathUPP; external name '_NewATSCubicClosePathUPP';

{
 *  DisposeATSCubicClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
procedure DisposeATSCubicClosePathUPP( userUPP: ATSCubicClosePathUPP ); external name '_DisposeATSCubicClosePathUPP';

{
 *  InvokeATSCubicClosePathUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function InvokeATSCubicClosePathUPP( callBackDataPtr: UnivPtr; userUPP: ATSCubicClosePathUPP ): OSStatus; external name '_InvokeATSCubicClosePathUPP';

{
 *  ATSUGlyphGetCubicPaths()
 *  
 *  Summary:
 *    Uses a callback mechanism to obtain a set of Cubic outlines for a
 *    specified glyph in a specified font.
 *  
 *  Discussion:
 *    This function will allow you to use callbacks to obtain the exact
 *    outline of a specified glyph, in cubic form. Although this
 *    function will always return results for any valid ATSUI font, you
 *    should first use the function ATSUGetNativeCurveType to determine
 *    the native format of the glyph you are interested in. Then,
 *    either call ATSUGlyphGetQuadraticPaths or ATSUGlyphGetCubicPaths
 *    based on the result. Otherwise, you may end up with curves that
 *    are mathematically converted from quadratic to cubic (or vice
 *    versa), instead of getting native font data. See the definitions
 *    of ATSCubicMoveToProcPtr, ATSCubicLineToProcPtr,
 *    ATSCubicCurveToProcPtr, and ATSCubicClosePathProcPtr for more
 *    information about setting up the callbacks.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referring to a font you wish to obtain a set of glyph
 *      outlines from.
 *    
 *    iGlyphID:
 *      A ID number referring to the glyph for which you wish to obtain
 *      outline data. Use the ATSUI direct access functions in
 *      ATSUnicodeDirectAccess.h to obtain values to pass for this
 *      parameter.
 *    
 *    iMoveToProc:
 *      A pointer to a callback function for cubic MoveTo operations.
 *      See the definition of ATSCubicMoveToProcPtr for more
 *      information about creating, disposing, and invoking this type
 *      of Universal Procedure Pointer.
 *    
 *    iLineToProc:
 *      A pointer to a callback function for cubic LineTo operations.
 *      See the definition of ATSCubicLineToProcPtr for more
 *      information about creating, disposing, and invoking this type
 *      of Universal Procedure Pointer.
 *    
 *    iCurveToProc:
 *      A pointer to a callback function for cubic CurveTo operations.
 *      See the definition of ATSCubicCurveToProcPtr for more
 *      information about creating, disposing, and invoking this type
 *      of Universal Procedure Pointer.
 *    
 *    iClosePathProc:
 *      A pointer to a callback function for cubic MoveTo operations.
 *      See the definition of ATSCubicClosePathProcPtr for more
 *      information about creating, disposing, and invoking this type
 *      of Universal Procedure Pointer.
 *    
 *    iCallbackDataPtr:
 *      Any valid pointer. Any application specific data you wish to
 *      pass to your callbacks may be sent through this parameter. can be NULL
 *    
 *    oCallbackResult:
 *      On return, status returned by callback functions. If an error
 *      occurs, callbacks may communicate it through this parameter.
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGlyphGetCubicPaths( iATSUStyle: ATSUStyle; iGlyphID: GlyphID; iMoveToProc: ATSCubicMoveToUPP; iLineToProc: ATSCubicLineToUPP; iCurveToProc: ATSCubicCurveToUPP; iClosePathProc: ATSCubicClosePathUPP; iCallbackDataPtr: UnivPtr; var oCallbackResult: OSStatus ): OSStatus; external name '_ATSUGlyphGetCubicPaths';


{
 *  ATSUGlyphGetCurvePaths()
 *  
 *  Summary:
 *    Obtains glyph curve data without the use of callbacks.
 *  
 *  Discussion:
 *    This function will return glyph curve data in a single data
 *    structure rather than through the use of callbacks, but you must
 *    parse the data structure yourself. ATSUGlyphGetCubicPaths and
 *    ATSUGlyphGetQuadraticPaths will parse the glyph data for you and
 *    use the callbacks you provide them to give you access to the
 *    individual points on the curves. Typically you use the function
 *    ATSUGlyphGetCurvePaths by calling it twice, as follows: (1) Pass
 *    a valid style and glyphID into the iATSUStyle and iGlyphID
 *    parameters, respectively, 0 for the ioBufferSize parameter, and
 *    NULL for the oPaths parameter. ATSUGlyphGetCurvePaths returns the
 *    size to use for the oPaths array in the ioBufferSize parameter.
 *    (2) Allocate enough space an array of the returned size, then
 *    call the ATSUGlyphGetCurvePaths again, passing a pointer to the
 *    array in the oPaths parameter. On return, the array contains the
 *    glyph outline data.
 *  
 *  Parameters:
 *    
 *    iATSUStyle:
 *      A style referring to a font you wish to obtain a set of glyph
 *      outlines from.
 *    
 *    iGlyphID:
 *      A ID number referring to the glyph for which you wish to obtain
 *      outline data. Use the ATSUI direct access functions in
 *      ATSUnicodeDirectAccess.h to obtain values to pass for this
 *      parameter.
 *    
 *    ioBufferSize:
 *      On input, the size of the buffer you have allocated for the
 *      oPaths parameter. On return, the actual size of the data
 *      structure that has been copied into the oPaths parameter.
 *    
 *    oPaths:
 *      On return, a data structure containing glyph outline
 *      information. See ATSTypes.h for a definition of this data
 *      structure. can be NULL
 *  
 *  Result:
 *    On success, noErr is returned. See MacErrors.h for possible error
 *    codes.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGlyphGetCurvePaths( iATSUStyle: ATSUStyle; iGlyphID: GlyphID; ioBufferSize: ByteCountPtr; oPaths: ATSUCurvePathsPtr ): OSStatus; external name '_ATSUGlyphGetCurvePaths';


{ Functions listed beyond this point are either deprecated or not recommended }

{ ---------------------------------------------------------------------------- }
{ ATSUI glyphInfo access (deprecated)                                          }
{ ---------------------------------------------------------------------------- }
{
 *  ATSUGetGlyphInfo()
 *  
 *  Summary:
 *    Obtains a copy of the style and layout information for each glyph
 *    in a line.
 *  
 *  Discussion:
 *    Please see ATSUnicodeDirectAccess.h for replacement functions.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUGetGlyphInfo( iTextLayout: ATSUTextLayout; iLineStart: UniCharArrayOffset; iLineLength: UniCharCount; ioBufferSize: ByteCountPtr; oGlyphInfoPtr: ATSUGlyphInfoArrayPtr ): OSStatus; external name '_ATSUGetGlyphInfo';


{
 *  ATSUDrawGlyphInfo()
 *  
 *  Summary:
 *    Draws glyphs at the specified location, based on style and layout
 *    information specified for each glyph.
 *  
 *  Discussion:
 *    Please see ATSUnicodeDirectAccess.h for replacement functions.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   in ATSUnicodeLib 9.1 and later
 }
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER
function ATSUDrawGlyphInfo( iGlyphInfoArray: ATSUGlyphInfoArrayPtr; iLocation: Float32Point ): OSStatus; external name '_ATSUDrawGlyphInfo';

end.

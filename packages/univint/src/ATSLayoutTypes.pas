{
     File:       ATS/ATSLayoutTypes.h
 
     Contains:   Apple Type Services layout public structures and constants.
 
     Version:    ATS
 
     Copyright:  © 1994-2012 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}

{ Pascal Translation Updated: Gorazd Krosl <gorazd_1957@yahoo.ca>, October 2009 }
{ Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }

{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit ATSLayoutTypes;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,SFNTLayoutTypes,ATSTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


{ --------------------------------------------------------------------------- }
{ CONSTANTS and related scalar types }
{ --------------------------------------------------------------------------- }
{ --------------------------------------------------------------------------- }
{ Miscellaneous Constants }
{ --------------------------------------------------------------------------- }
const
	kATSUseGlyphAdvance = $7FFFFFFF; { assignment to use natural glyph advance value }
	kATSUseLineHeight = $7FFFFFFF; { assignment to use natural line ascent/descent values }
	kATSNoTracking = $80000000; { negativeInfinity }

{ --------------------------------------------------------------------------- }

{
 *  Summary:
 *    These values are passed into the ATSUGetGlyphBounds function to
 *    indicate whether the width of the resulting typographic glyph
 *    bounds will be determined using the caret origin, glyph origin in
 *    device space, or glyph origin in fractional absolute positions
 }
const
{
   * Specifies that the width of the typographic glyph bounds will be
   * determined using the caret origin. The caret origin is halfway
   * between two characters.
   }
	kATSUseCaretOrigins = 0;

  {
   * Specifies that the width of the typographic glyph bounds will be
   * determined using the glyph origin in device space. This is useful
   * for adjusting text on the screen.
   }
	kATSUseDeviceOrigins = 1;

  {
   * Specifies that the width of the typographic glyph bounds will be
   * determined using the glyph origin in fractional absolute
   * positions, which are uncorrected for device display. This provides
   * the ideal position of laid-out text and is useful for scaling text
   * on the screen. This origin is also used to get the width of the
   * typographic bounding rectangle when you call ATSUMeasureText.
   }
	kATSUseFractionalOrigins = 2;
	kATSUseOriginFlags = 3;

{ ---------------------------------------------------------------------------- }

{
 *  ATSULayoutOperationSelector
 *  
 *  Summary:
 *    This is used to select which operations to override, or which
 *    operation is currently being run.
 }
type
	ATSULayoutOperationSelector = UInt32;
const
{
   * No Layout operation is currently selected.
   }
	kATSULayoutOperationNone = $00000000;

  {
   * Select the Justification operation.
   }
	kATSULayoutOperationJustification = $00000001;

  {
   * Select the character morphing operation.
   }
	kATSULayoutOperationMorph = $00000002;

  {
   * Select the kerning adjustment operation.
   }
	kATSULayoutOperationKerningAdjustment = $00000004;

  {
   * Select the baseline adjustment operation.
   }
	kATSULayoutOperationBaselineAdjustment = $00000008;

  {
   * Select the tracking adjustment operation.
   }
	kATSULayoutOperationTrackingAdjustment = $00000010;

  {
   * Select the period of time after ATSUI has finished all of it's
   * layout operations.
   }
	kATSULayoutOperationPostLayoutAdjustment = $00000020;
	kATSULayoutOperationAppleReserved = $FFFFFFC0;

{ ---------------------------------------------------------------------------- }

{
 *  ATSULayoutOperationCallbackStatus
 *  
 *  Summary:
 *    One of these must be returned by a
 *    ATSUDLayoutOperationOverrideUPP callback function in order to
 *    indicate ATSUI's status.
 }
type
	ATSULayoutOperationCallbackStatus = UInt32;
const
{
   * Return this if the callback function has totally handled the
   * operation which triggered the callback and does not need ATSUI to
   * run any further processing for the operation.
   }
	kATSULayoutOperationCallbackStatusHandled = $00000000;

  {
   * Return this if the callback function has not totally handled the
   * operation which triggered the callback and needs ATSUI to run it's
   * own processing.
   }
	kATSULayoutOperationCallbackStatusContinue = $00000001;

{ ---------------------------------------------------------------------------- }

{
 *  ATSLineLayoutOptions
 *  
 *  Summary:
 *    ATSLineLayoutOptions are normally set in an ATSUTextLayout object
 *    via the kATSULineLayoutOptionsTag layout control attribute. They
 *    can also be set in an ATSLineLayoutParams structure.
 }
type
	ATSLineLayoutOptions = UInt32;
const
{
   * No options specified.
   }
	kATSLineNoLayoutOptions = $00000000;

  {
   * This line option is no longer used.
   }
	kATSLineIsDisplayOnly = $00000001; { obsolete option}

  {
   * Specifies that no hangers to be formed on the line.
   }
	kATSLineHasNoHangers = $00000002;

  {
   * Specifies that no optical alignment to be performed on the line.
   }
	kATSLineHasNoOpticalAlignment = $00000004;

  {
   * Specifies that space charcters should not be treated as hangers.
   }
	kATSLineKeepSpacesOutOfMargin = $00000008;

  {
   * Specifies no post-compensation justification is to be performed.
   }
	kATSLineNoSpecialJustification = $00000010;

  {
   * Specifies that if the line is the last of a paragraph, it will not
   * get justified.
   }
	kATSLineLastNoJustification = $00000020;

  {
   * Specifies that the displayed line glyphs will adjust for device
   * metrics.
   }
	kATSLineFractDisable = $00000040;

  {
   * Specifies that the carets at the ends of the line will be
   * guarenteed to be perpendicular to the baseline.
   }
	kATSLineImposeNoAngleForEnds = $00000080;

  {
   * Highlights for the line end characters will be extended to 0 and
   * the specified line width.
   }
	kATSLineFillOutToWidth = $00000100;

  {
   * Specifies that the tab character width will be automatically
   * adjusted to fit the specified line width.
   }
	kATSLineTabAdjustEnabled = $00000200;

  {
   * Specifies that any leading value specified by a font will be
   * ignored.
   }
	kATSLineIgnoreFontLeading = $00000400;

  {
   * Specifies that ATS produce antialiased glyph images despite system
   * preferences (negates kATSLineNoAntiAliasing bit if set).
   }
	kATSLineApplyAntiAliasing = $00000800;

  {
   * Specifies that ATS turn-off antialiasing glyph imaging despite
   * system preferences (negates kATSLineApplyAntiAliasing bit if set).
   }
	kATSLineNoAntiAliasing = $00001000;

  {
   * Specifies that if the line width is not sufficient to hold all its
   * glyphs, glyph positions are allowed to extend beyond the line's
   * assigned width so negative justification is not used.
   }
	kATSLineDisableNegativeJustification = $00002000;

  {
   * Specifies that lines with any integer glyph positioning (due to
   * either any character non-antialiased or kATSLineFractDisable
   * specified), not automatically esthetically adjust individual
   * character positions while rendering to display.
   }
	kATSLineDisableAutoAdjustDisplayPos = $00004000;

  {
   * Specifies that rendering be done simulating Quickdraw rendering
   * (4-bit pixel aligned antialiasing).
   }
	kATSLineUseQDRendering = $00008000;

  {
   * Specifies that any Justification operations will not be run.
   }
	kATSLineDisableAllJustification = $00010000;

  {
   * Specifies that any glyph morphing operations will not be run.
   }
	kATSLineDisableAllGlyphMorphing = $00020000;

  {
   * Specifies that any kerning adjustment operations will not be run.
   }
	kATSLineDisableAllKerningAdjustments = $00040000;

  {
   * Specifies that any baseline adjustment operations will not be run.
   }
	kATSLineDisableAllBaselineAdjustments = $00080000;

  {
   * Specifies that any tracking adjustment operations will not be run.
   }
	kATSLineDisableAllTrackingAdjustments = $00100000;

  {
   * Convenience constant for turning-off all adjustments.
   }
	kATSLineDisableAllLayoutOperations = kATSLineDisableAllJustification or kATSLineDisableAllGlyphMorphing or kATSLineDisableAllKerningAdjustments or kATSLineDisableAllBaselineAdjustments or kATSLineDisableAllTrackingAdjustments;

  {
   * Specifies to optimize for displaying text only.  Note, rounded
   * device metrics will be used instead of fractional path metrics.
   }
	kATSLineUseDeviceMetrics = $01000000;

  {
   * Specifies that line breaking should occur at the nearest
   * character, not word.  This could cause a word to be split among
   * multiple lines.
   }
	kATSLineBreakToNearestCharacter = $02000000;

  {
   * These bits are reserved by Apple and will result in a invalid
   * value error if attemped to set. Obsolete constants:
   }
	kATSLineAppleReserved = $FCE00000;

{ --------------------------------------------------------------------------- }

{
 *  ATSStyleRenderingOptions
 *  
 *  Summary:
 *    ATSStyleRenderingOptions are set in the ATSUStyle object via the
 *    attribute tag kATSUStyleRenderingOptions. They provide finer
 *    control over how the style is rendered.
 }
type
	ATSStyleRenderingOptions = UInt32;
const
{
   * No options specified.
   }
	kATSStyleNoOptions = $00000000;

  {
   * Specifies that ATS produce "unhinted" glyph outlines (default is
   * hinted glyph outlines).
   }
	kATSStyleNoHinting = $00000001;

  {
   * Specifies that ATS produce antialiased glyph images despite system
   * preferences or LineLayoutOptions (negates kATSStyleNoAntiAliasing
   * bit if set).
   }
	kATSStyleApplyAntiAliasing = $00000002;

  {
   * Specifies that ATS turn-off antialiasing glyph imaging despite
   * system preferences or LineLayoutOptions (negates
   * kATSStyleApplyAntiAliasing bit if set).
   }
	kATSStyleNoAntiAliasing = $00000004;

  {
   * These bits are reserved by Apple and will result in a invalid
   * value error if attemped to set.
   }
	kATSStyleAppleReserved = $FFFFFFF8;

  {
   * (OBSOLETE) Specifies that ATS produce "hinted" glyph outlines (the
   * default behavior). THIS NAME IS OBSOLETE. DO NOT USE. It's only
   * left in for backwards compatibility.
   }
	kATSStyleApplyHints = kATSStyleNoOptions;

{ --------------------------------------------------------------------------- }


{
 *  ATSGlyphInfoFlags
 *  
 *  Summary:
 *    ATSGlyphInfoFlags are set in the individual ATSLayoutRecord
 *    structures and apply only to the ATSGlyphRef in that structure.
 *    The are used by the layout engine to flag a glyph with specific
 *    properties.
 }
type
	ATSGlyphInfoFlags = UInt32;
const
{
   * These bits are Apple reserved and may result in an invalid value
   * error if attempted to set.
   }
	kATSGlyphInfoAppleReserved = $1FFBFFE8;

  {
   * The glyph attaches to another glyph.
   }
	kATSGlyphInfoIsAttachment = $80000000;

  {
   * The glyph can hang off left/top edge of line.
   }
	kATSGlyphInfoIsLTHanger = $40000000;

  {
   * The glyph can hang off right/bottom edge of line.
   }
	kATSGlyphInfoIsRBHanger = $20000000;

  {
   * The glyph is not really a glyph at all, but an end-marker designed
   * to allow the calculation of the previous glyph's advance.
   }
	kATSGlyphInfoTerminatorGlyph = $00080000;

  {
   * The glyph is a white space glyph.
   }
	kATSGlyphInfoIsWhiteSpace = $00040000;

  {
   * Glyph has a style specified imposed width (i.e. advance)
   }
	kATSGlyphInfoHasImposedWidth = $00000010;

  {
   * A three-bit mask, that can be used to get the size of the original
   * character that spawned this glyph. When a logical 'and' operation
   * with this mask and an ATSGlyphInfoFlags variable, it will yield
   * the size in bytes of the original character (0 - 7 bytes possible).
   }
	kATSGlyphInfoByteSizeMask = $00000007;

{ --------------------------------------------------------------------------- }
{ STRUCTURED TYPES and related constants }
{ --------------------------------------------------------------------------- }

{
 *  ATSLayoutRecord
 *  
 *  Summary:
 *    The ATSLayoutRecord structure defines all the needed info for a
 *    single glyph during the layout process.  This struct must be
 *    declared as the first element of an enclosing glyph record struct
 *    defined by ATSUI DirectAccess clients.
 }
type
	ATSLayoutRecord = record
{
   * The glyph ID reference.
   }
		glyphID: ATSGlyphRef;

  {
   * These flags describe the individual state of the glyph (see above).
   }
		flags: ATSGlyphInfoFlags;

  {
   * The byte offset of the original character that spawned this glyph.
   }
		originalOffset: ByteCount;

  {
   * This is the real position that the glyph sits.
   }
		realPos: Fixed;
	end;
	ATSLayoutRecordPtr = ^ATSLayoutRecord;
	
{ --------------------------------------------------------------------------- }

{
 *  ATSTrapezoid
 *  
 *  Summary:
 *    The ATSTrapezoid structure supplies a convenient container for
 *    glyph bounds in trapezoidal form.
 }
type
	ATSTrapezoid = record
		upperLeft: FixedPoint;
		upperRight: FixedPoint;
		lowerRight: FixedPoint;
		lowerLeft: FixedPoint;
	end;
	ATSTrapezoidPtr = ^ATSTrapezoid;
	
{ --------------------------------------------------------------------------- }

{
 *  ATSJustWidthDeltaEntryOverride
 *  
 *  Summary:
 *    The JustWidthDeltaEntryOverride structure specifies values for
 *    the grow and shrink case during justification, both on the left
 *    and on the right. It also contains flags.  This particular
 *    structure is used for passing justification overrides to LLC. For
 *    further sfnt resource 'just' table constants and structures, see
 *    SFNTLayoutTypes.h.
 }
type
	ATSJustWidthDeltaEntryOverride = record
{
   * ems AW can grow by at most on LT
   }
		beforeGrowLimit: Fixed;

  {
   * ems AW can shrink by at most on LT
   }
		beforeShrinkLimit: Fixed;

  {
   * ems AW can grow by at most on RB
   }
		afterGrowLimit: Fixed;

  {
   * ems AW can shrink by at most on RB
   }
		afterShrinkLimit: Fixed;

  {
   * flags controlling grow case
   }
		growFlags: JustificationFlags;

  {
   * flags controlling shrink case
   }
		shrinkFlags: JustificationFlags;
	end;
	ATSJustWidthDeltaEntryOverridePtr = ^ATSJustWidthDeltaEntryOverride;
	
{ The JustPriorityOverrides type is an array of 4 width delta records, one per priority level override. }
	ATSJustPriorityWidthDeltaOverrides = array[0..3] of ATSJustWidthDeltaEntryOverride;
	
{ ---------------------------------------------------------------------------- }

{
 *  ATSULineRef
 *  
 *  Summary:
 *    A reference to a line that is being laid out. This is passed into
 *    the ATSUDirectLayoutOperationOverrideUPP callback function to be
 *    used by the ATSUDirectGetLayoutDataArrayPtrFromLineRef function.
 *    The only way to get a line ref is inside of the callback. The
 *    only time the line ref is valid is inside of the callback.
 }
type
	ATSGlyphVector = record end;
	ATSULineRef = ^ATSGlyphVector;
{ ---------------------------------------------------------------------------- }
{ DirectAccess Layout Callback Definitions                                     }
{ ---------------------------------------------------------------------------- }

{
 *  ATSUDirectLayoutOperationOverrideProcPtr
 *  
 *  Summary:
 *    Callback definition for a low-level adjustment routine hook.
 *  
 *  Discussion:
 *    This callback can be set in an ATSUTextLayout object by setting
 *    the attribute tag kATSULayoutOperationOverrideTag and passing in
 *    a ATSULayoutOperationOverrideSpecifier structure into
 *    ATSUSetLayoutAttribute. This callback will be called whenever an
 *    ATSUI call triggers a re-layout for each operation it is
 *    installed for. The operation that triggered the callback will be
 *    set in the iCurrentOperation parameter. The callback function
 *    defined by the developer is only required to do one thing: return
 *    it's status to ATSUI as to what it has done. This is done via the
 *    oCallbackStatus parameter. It needs to tell ATSUI if it had
 *    handled the layout operation or if it still needs ATSUI to run
 *    it's own processes. iOperationCallbackParameterPtr is there in
 *    case there are ever any ATSUDirectLayoutOperationSelector which
 *    require extra parameters to be passed into the callback function.
 *    It is currently unused and will always be set to NULL. iRefCon is
 *    the constant that is set in the ATSUTextLayout object that
 *    spawned the operation by the ATSUSetTextLayoutRefCon() API.
 *    Within the context of the callback itself, only a limited subset
 *    of ATSUI APIs may be called. Basically, only the APIs that have
 *    no chance of triggering a re-layout are allowed to be called. The
 *    reason for this restriction is to prevent runaway recursion. Most
 *    of the APIs that have "create", "get", or "copy" are safe. Any
 *    attempt to call one of the restricted APIs will result in an
 *    immediate return with the kATSUInvalidCallInsideCallbackErr
 *    error. ATSULayoutOperationSelector and
 *    ATSULayoutOperationCallbackStatus are defined in ATSLayoutTypes.i.
 }
type
	ATSUDirectLayoutOperationOverrideProcPtr = function( iCurrentOperation: ATSULayoutOperationSelector; iLineRef: ATSULineRef; iRefCon: URefCon; iOperationCallbackParameterPtr: UnivPtr; var oCallbackStatus: ATSULayoutOperationCallbackStatus ): OSStatus;
	ATSUDirectLayoutOperationOverrideUPP = ATSUDirectLayoutOperationOverrideProcPtr;

{
 *  NewATSUDirectLayoutOperationOverrideUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewATSUDirectLayoutOperationOverrideUPP( userRoutine: ATSUDirectLayoutOperationOverrideProcPtr ): ATSUDirectLayoutOperationOverrideUPP; external name '_NewATSUDirectLayoutOperationOverrideUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)

{
 *  DisposeATSUDirectLayoutOperationOverrideUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeATSUDirectLayoutOperationOverrideUPP( userUPP: ATSUDirectLayoutOperationOverrideUPP ); external name '_DisposeATSUDirectLayoutOperationOverrideUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)

{
 *  InvokeATSUDirectLayoutOperationOverrideUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeATSUDirectLayoutOperationOverrideUPP( iCurrentOperation: ATSULayoutOperationSelector; iLineRef: ATSULineRef; iRefCon: URefCon; iOperationCallbackParameterPtr: UnivPtr; var oCallbackStatus: ATSULayoutOperationCallbackStatus; userUPP: ATSUDirectLayoutOperationOverrideUPP ): OSStatus; external name '_InvokeATSUDirectLayoutOperationOverrideUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)

{
#if __MACH__
    #define NewATSUDirectLayoutOperationOverrideUPP(userRoutine) ((ATSUDirectLayoutOperationOverrideUPP)userRoutine)
    #define DisposeATSUDirectLayoutOperationOverrideUPP(userUPP)
    #define InvokeATSUDirectLayoutOperationOverrideUPP(iCurrentOperation, iLineRef, iRefCon, iOperationCallbackParameterPtr, oCallbackStatus, userUPP) (*userUPP)(iCurrentOperation, iLineRef, iRefCon, iOperationCallbackParameterPtr, oCallbackStatus)
#endif
}

{ ---------------------------------------------------------------------------- }

{
 *  ATSULayoutOperationOverrideSpecifier
 *  
 *  Summary:
 *    This structure is used to install a callback for one or more
 *    ATSUI operations. To do this, simply passed one of these
 *    structure into the ATSUSetLayoutControls call with the
 *    kATSULayoutOperationOverrideTag tag.
 }
type
	ATSULayoutOperationOverrideSpecifier = record
{
   * A bitfield containing the selector for the operations in which the
   * callback will be installed for.
   }
		operationSelector: ATSULayoutOperationSelector;
		overrideUPP: ATSUDirectLayoutOperationOverrideUPP;
	end;
	ATSULayoutOperationOverrideSpecifierPtr = ^ATSULayoutOperationOverrideSpecifier;
	
//#pragma pack(pop)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

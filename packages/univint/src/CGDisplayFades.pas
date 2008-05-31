{
 *  CGDisplayFade.h
 *  CoreGraphics
 *
 *  API to fade displays to and from a solid color, without resorting
 *  to playing with the gamma table APIs and losing ColorSync calibration.
 *
 *  These APIs should be used in perference to manipulating the gamma tables
 *  for purposes of performing fade effects.
 *
 *  Copyright (c) 2002 Apple Computer, Inc. All rights reserved.
 *
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

unit CGDisplayFades;
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
uses MacTypes,CGBase,CGErrors,CGDirectDisplay,CGDisplayConfiguration;
{$ALIGN POWER}


type
	CGDisplayFadeReservationToken = UInt32;
const
	kCGDisplayFadeReservationInvalidToken = 0;

type
	CGDisplayBlendFraction = Float32;
{
 * Values for the limits of the fade.
 *	kCGDisplayBlendNormal represents a normal display state
 *	kCGDisplayBlendSolidColor represents a display blended to a solid color
 }
const
	kCGDisplayBlendNormal = 0.0;
const
	kCGDisplayBlendSolidColor = 1.0;

{
 * Time in seconds to perform a fade operation.
 }
type
	CGDisplayFadeInterval = Float32;

{
 *
 * Most fade operations done by apps and games are done around display
 * configuration changes.  This API adds control over a built-in fade
 * effect when performing display configuration changes.
 *
 * The default fade effect on a display mode change uses a fade-out of
 * 0.3 seconds and a fade-in of 0.5 seconds.  Color fades to French Blue
 * for a normal desktop, and black when displays are captured.
 *
 * CGConfigureDisplayFadeEffect sets the display fade time and color
 * for a display reconfigure operation.
 * Call after CGBeginDisplayConfiguration() and before
 * calling CGCompleteDisplayConfiguration().
 *
 * When CGCompleteDisplayConfiguration() is called, a fade-out effect will be
 * done prior to the display reconfiguration.  When the display reconfiguration
 * is complete, control returns to the calling program, while a fade-in effect
 * runs asynchronously.
 }
function CGConfigureDisplayFadeEffect( configRef: CGDisplayConfigRef; fadeOutSeconds: CGDisplayFadeInterval; fadeInSeconds: CGDisplayFadeInterval; fadeRed: Float32; fadeGreen: Float32; fadeBlue: Float32 ): CGError; external name '_CGConfigureDisplayFadeEffect';

{
 * It may also be desirable to perform fade operations at other times, as when
 * transitioning between game play and cinematic sequences.  The following API
 * provides a mechanism for controlling display fade operations outside of display
 * mode reconfigurations.
 }
type
	CGDisplayReservationInterval = Float32;
const
	kCGMaxDisplayReservationInterval = 15.0;
{
 * Before performing fade operation, the caller must reserve the hardware
 * for the expected period of time that the program will be doing fades
 *
 * A reservation token is returned that must be passed in on subsequent calls.
 *
 * Failing to release the hardware by the end of the reservation interval will
 * result in the reservation token becomingn invalid, and the hardware being
 * unfaded back to a normal state.  The reservation interval is limited (clipped)
 * to 15 seconds maximum, and should be greater than zero.
 *
 * Returns kCGErrorNoneAvailable if another reservation is in effect,
 * and kCGErrorSuccess on success.
 }
function CGAcquireDisplayFadeReservation( seconds: CGDisplayReservationInterval; var pNewToken: CGDisplayFadeReservationToken ): CGError; external name '_CGAcquireDisplayFadeReservation';

{
 * Releases a display fade reservation, and unfades the display if needed
 * The reservation token myToken is no longer valid after this operation.
 *
 * CGReleaseDisplayFadeReservation may be safely called while an async fade
 * operation is running, and if the ending blend value is kCGDisplayBlendNormal,
 * will not disturb the running operation.  The reservation is dropped when the
 * fade opertion completes.
 *
 * Returns kCGErrorIllegalArgument if myToken is not the valid reservation token,
 * and kCGErrorSuccess on success.
 }
function CGReleaseDisplayFadeReservation( myToken: CGDisplayFadeReservationToken ): CGError; external name '_CGReleaseDisplayFadeReservation';

{
 * The actual fade mechanism:
 *
 * The function takes the current reservation token,
 * a time interval to perform the fade operation in seconds,
 * a starting and ending blend coefficient, an RGB color in device space,
 * and a boolean to indicate that the operation should be done synchronously.
 *
 * Over the fade operation time interval, the system will interpolate a
 * blending coefficient between the starting and ending values given,
 * applying a nonlinear (sine-based) bias term, and will blend the video output
 * with the specified color based on the resulting value.
 *
 * If the time interval is specifed as 0.0, then the ending state blend value is
 * applied at once and the function returns.
 *
 * The maximum allowable time interval is 15 seconds.
 *
 * If the parameter 'synchronous' is true, the function does not return
 * til the fade operation is complete.  If false, the function returns at once,
 * and the fade operation runs asynchronously.
 *
 * CGReleaseDisplayFadeReservation may be safely called while an async fade
 * operation is running, and if the ending blend value is kCGDisplayBlendNormal,
 * will not disturb the running operation.  The reservation is dropped when the
 * fade opertion completes.
 *
 * Invalid parameters result in a return value of kCGErrorIllegalArgument.
 * Trying to start a fade operation while an asynchronous fade operation is running
 * results in a return value of kCGErrorNoneAvailable.
 *
 * To perform a 2 second fade to black, waiting til complete:
 *
 *	CGDisplayFade(myToken,
 *      	      2.0,			// 2 seconds 
 *		      kCGDisplayBlendNormal,	// Starting state 
 *		      kCGDisplayBlendSolidColor, // Ending state 
 *		      0.0, 0.0, 0.0,		// black 
 *		      true);			// Wait for completion 
 *
 * To perform a 2 second fade from black to normal, without waiting for completion:
 *
 *	CGDisplayFade(myToken,
 *      	      2.0,			// 2 seconds 
 *		      kCGDisplayBlendSolidColor, // Starting state 
 *		      kCGDisplayBlendNormal,	// Ending state 
 *		      0.0, 0.0, 0.0,		// black 
 *		      false);			// Don't wait for completion 
 }
function CGDisplayFade( myToken: CGDisplayFadeReservationToken; seconds: CGDisplayFadeInterval; startBlend: CGDisplayBlendFraction; endBlend: CGDisplayBlendFraction; redBlend: Float32; greenBlend: Float32; blueBlend: Float32; synchronous: boolean_t ): CGError; external name '_CGDisplayFade';

{
 * Returns true if a fade operation is currently in progress.
 }
function CGDisplayFadeOperationInProgress: boolean_t; external name '_CGDisplayFadeOperationInProgress';


end.

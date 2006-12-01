{
 *  CGDirectDisplay.h
 *  CoreGraphics
 *
 *  Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
 *
 }
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

unit CGDirectDisplay;
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
uses MacTypes,CFBase,CFArray,CFDictionary,CGContext,CGBase,CGGeometry,CGErrors;
{$ALIGN POWER}


{
 * The following construct is present to avoid problems with some Apple tools.
 * API in this module is not available in Mac OS Classic variations!
 }


type
	CGDirectDisplayID = ^SInt32; { an opaque 32-bit type }
	CGDirectDisplayIDPtr = ^CGDirectDisplayID;  { when a var xx:CGDirectDisplayID parameter can be nil, it is changed to xx: CGDirectDisplayIDPtr }
type
	CGDirectPaletteRef = ^SInt32; { an opaque 32-bit type }
	CGDirectPaletteRefPtr = ^CGDirectPaletteRef;  { when a var xx:CGDirectPaletteRef parameter can be nil, it is changed to xx: CGDirectPaletteRefPtr }
type
	CGDisplayCount = UInt32;
type
	CGTableCount = UInt32;
type
	CGDisplayCoord = SInt32;
type
	CGByteValue = SInt8;
	CGByteValuePtr					= ^CGByteValue;
type
	CGOpenGLDisplayMask = UInt32;
type
	CGBeamPosition = UInt32;
type
	CGMouseDelta = SInt32;
type
	CGRefreshRate = double;
type
	CGCaptureOptions = UInt32;

type
	CGDisplayErr = CGError;

const
	CGDisplayNoErr				= kCGErrorSuccess;

	kCGDirectMainDisplay		= nil;

{ Returns the display ID of the current main display }
function CGMainDisplayID: CGDirectDisplayID; external name '_CGMainDisplayID';

{
 * Mechanisms used to find screen IDs
 * An array length (maxDisplays) and array of CGDirectDisplayIDs are passed in.
 * Up to maxDisplays of the array are filled in with the displays meeting the
 * specified criteria.  The actual number of displays filled in is returned in
 * dspyCnt.
 *
 * If the dspys array is NULL, maxDisplays is ignored, and *dspyCnt is filled
 * in with the number of displays meeting the function's requirements.
 }
function CGGetDisplaysWithPoint( point: CGPoint; maxDisplays: CGDisplayCount; dspys: CGDirectDisplayIDPtr; var dspyCnt: CGDisplayCount ): CGDisplayErr; external name '_CGGetDisplaysWithPoint';

function CGGetDisplaysWithRect( rect: CGRect; maxDisplays: CGDisplayCount; dspys: CGDirectDisplayIDPtr; var dspyCnt: CGDisplayCount ): CGDisplayErr; external name '_CGGetDisplaysWithRect';

function CGGetDisplaysWithOpenGLDisplayMask( mask: CGOpenGLDisplayMask; maxDisplays: CGDisplayCount; dspys: CGDirectDisplayIDPtr; var dspyCnt: CGDisplayCount ): CGDisplayErr; external name '_CGGetDisplaysWithOpenGLDisplayMask';
                            
{
 * Get lists of displays.  Use this to determine display IDs
 *
 * If the activeDspys array is NULL, maxDisplays is ignored, and *dspyCnt is filled
 * in with the number of displays meeting the function's requirements.
 *
 * The first display returned in the list is the main display,
 * the one with the menu bar.
 * When mirroring, this will be the largest drawable display in the mirror,
 * set, or if all are the same size, the one with the deepest pixel depth.
 }
function CGGetActiveDisplayList( maxDisplays: CGDisplayCount; activeDspys: CGDirectDisplayIDPtr; var dspyCnt: CGDisplayCount ): CGDisplayErr; external name '_CGGetActiveDisplayList';

{
 * With hardware mirroring, a display may be on-line,
 * but not necessarily active, or drawable.
 * Programs which manipulate display settings such as the
 * palette or gamma tables need access to all displays in use,
 * including hardware mirrors which are not drawable.
 }
function CGGetOnlineDisplayList( maxDisplays: CGDisplayCount; onlineDspys: CGDirectDisplayIDPtr; var dspyCnt: CGDisplayCount ): CGDisplayErr; external name '_CGGetOnlineDisplayList';

{ Map a display to an OpenGL display mask; returns 0 on invalid display }
function CGDisplayIDToOpenGLDisplayMask( display: CGDirectDisplayID ): CGOpenGLDisplayMask; external name '_CGDisplayIDToOpenGLDisplayMask';

{
 * Map an OpenGL display mask to a display.
 * Returns kCGNullDirectDisplay if a bit doesn't
 * match a display.
 * Passing in multiple bits results in an arbitrary match. 
 }
function CGOpenGLDisplayMaskToDisplayID( mask: CGOpenGLDisplayMask ): CGDirectDisplayID; external name '_CGOpenGLDisplayMaskToDisplayID';

{ Return screen size and origin in global coords; Empty rect if display is invalid }
function CGDisplayBounds( display: CGDirectDisplayID ): CGRect; external name '_CGDisplayBounds';

function CGDisplayPixelsWide( display: CGDirectDisplayID ): size_t; external name '_CGDisplayPixelsWide';
function CGDisplayPixelsHigh( display: CGDirectDisplayID ): size_t; external name '_CGDisplayPixelsHigh';

{
 * Display mode selection
 * Display modes are represented as CFDictionaries
 * All dictionaries and arrays returned via these mechanisms are
 * owned by the framework and should not be released.  The framework
 * will not release them out from under your application.
 *
 * Values associated with the following keys are CFNumber types.
 * With CFNumberGetValue(), use kCFNumberLongType for best results.
 * kCGDisplayRefreshRate encodes a double value, so to get the fractional
 * refresh rate use kCFNumberDoubleType.
 }
 
{
 * Keys used in mode dictionaries.  Source C strings shown won't change.
 * Some CFM environments cannot import data variables, and so
 * the definitions are provided directly.
 *
 * These keys are used only within the scope of the mode dictionaries,
 * so further uniquing, as by prefix, of the source string is not needed.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayWidth CFSTRP('Width')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayHeight CFSTRP('Height')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayMode CFSTRP('Mode')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayBitsPerPixel CFSTRP('BitsPerPixel')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayBitsPerSample CFSTRP('BitsPerSample')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplaySamplesPerPixel CFSTRP('SamplesPerPixel')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayRefreshRate CFSTRP('RefreshRate')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayModeUsableForDesktopGUI CFSTRP('UsableForDesktopGUI')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayIOFlags CFSTRP('IOFlags')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayBytesPerRow CFSTRP('kCGDisplayBytesPerRow')}
{$endc}

{
 * Keys to describe optional properties of display modes.
 *
 * The key will only be present if the property applies,
 * and will be associated with a value of kCFBooleanTrue.
 * Keys not relevant to a particular display mode will not
 * appear in the mode dictionary.
 *
 * These strings must remain unchanged in future releases, of course.
 }

{ Set if display mode doesn't need a confirmation dialog to be set }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayModeIsSafeForHardware CFSTRP('kCGDisplayModeIsSafeForHardware')}
{$endc}

{ The following keys reflect interesting bits of the IOKit display mode flags }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayModeIsInterlaced CFSTRP('kCGDisplayModeIsInterlaced')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayModeIsStretched CFSTRP('kCGDisplayModeIsStretched')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGDisplayModeIsTelevisionOutput CFSTRP('kCGDisplayModeIsTelevisionOutput')}
{$endc}


{
 * Return a CFArray of CFDictionaries describing all display modes.
 * Returns NULL if the display is invalid.
 }
function CGDisplayAvailableModes( display: CGDirectDisplayID ): CFArrayRef; external name '_CGDisplayAvailableModes';
{
 * Try to find a display mode of specified depth with dimensions equal or greater than
 * specified.
 * If no depth match is found, try for the next larger depth with dimensions equal or greater
 * than specified.  If no luck, then just return the current mode.
 *
 * exactmatch, if not NULL, is set to 'true' if an exact match in width, height, and depth is found,
 * and 'false' otherwise.
 *
 * CGDisplayBestModeForParametersAndRefreshRateWithProperty searches the list, looking for
 * display modes with the specified property.  The property should be one of:
 *	kCGDisplayModeIsSafeForHardware;
 *	kCGDisplayModeIsInterlaced;
 *	kCGDisplayModeIsStretched;
 *	kCGDisplayModeIsTelevisionOutput
 *	
 * Returns NULL if display is invalid.
 }
function CGDisplayBestModeForParameters( display: CGDirectDisplayID; bitsPerPixel: size_t; width: size_t; height: size_t; var exactMatch: boolean_t ): CFDictionaryRef; external name '_CGDisplayBestModeForParameters';

function CGDisplayBestModeForParametersAndRefreshRate( display: CGDirectDisplayID; bitsPerPixel: size_t; width: size_t; height: size_t; refresh: CGRefreshRate; var exactMatch: boolean_t ): CFDictionaryRef; external name '_CGDisplayBestModeForParametersAndRefreshRate';

function CGDisplayBestModeForParametersAndRefreshRateWithProperty( display: CGDirectDisplayID; bitsPerPixel: size_t; width: size_t; height: size_t; refresh: CGRefreshRate; property: CFStringRef; var exactMatch: boolean_t ): CFDictionaryRef; external name '_CGDisplayBestModeForParametersAndRefreshRateWithProperty';

{
 * Return a CFDictionary describing the current display mode.
 * Returns NULL if display is invalid.
 }
function CGDisplayCurrentMode( display: CGDirectDisplayID ): CFDictionaryRef; external name '_CGDisplayCurrentMode';
{
 * Switch display mode.  Note that after switching, 
 * display parameters and addresses may change.
 * The selected display mode persists for the life of the program, and automatically
 * reverts to the permanent setting made by Preferences when the program terminates.
 * The mode dictionary passed in must be a dictionary vended by other CGDirectDisplay
 * APIs such as CGDisplayBestModeForParameters() and CGDisplayAvailableModes().
 *
 * The mode dictionary passed in must be a dictionary vended by other CGDirectDisplay
 * APIs such as CGDisplayBestModeForParameters() and CGDisplayAvailableModes().
 *
 * When changing display modes of displays in a mirroring set, other displays in
 * the mirroring set will be set to a display mode capable of mirroring the bounds
 * of the largest display being explicitly set. 
 }
function CGDisplaySwitchToMode( display: CGDirectDisplayID; mode: CFDictionaryRef ): CGDisplayErr; external name '_CGDisplaySwitchToMode';

{ Query parameters for current mode }
function CGDisplayBitsPerPixel( display: CGDirectDisplayID ): size_t; external name '_CGDisplayBitsPerPixel';
function CGDisplayBitsPerSample( display: CGDirectDisplayID ): size_t; external name '_CGDisplayBitsPerSample';
function CGDisplaySamplesPerPixel( display: CGDirectDisplayID ): size_t; external name '_CGDisplaySamplesPerPixel';
function CGDisplayBytesPerRow( display: CGDirectDisplayID ): size_t; external name '_CGDisplayBytesPerRow';

{
 * Set a display gamma/transfer function from a formula specifying
 * min and max values and a gamma for each channel.
 * Gamma values must be greater than 0.0.
 * To get an antigamma of 1.6, one would specify a value of (1.0 / 1.6)
 * Min values must be greater than or equal to 0.0 and less than 1.0.
 * Max values must be greater than 0.0 and less than or equal to 1.0.
 * Out of range values, or Max greater than or equal to Min result
 * in a kCGSRangeCheck error.
 *
 * Values are computed by sampling a function for a range of indices from 0 through 1:
 *	value = Min + ((Max - Min) * pow(index, Gamma))
 * The resulting values are converted to a machine specific format
 * and loaded into hardware.
 }
type
	CGGammaValue = Float32;
	CGGammaValuePtr					= ^CGGammaValue;
                                              
function CGSetDisplayTransferByFormula( display: CGDirectDisplayID; redMin: CGGammaValue; redMax: CGGammaValue; redGamma: CGGammaValue; greenMin: CGGammaValue; greenMax: CGGammaValue; greenGamma: CGGammaValue; blueMin: CGGammaValue; blueMax: CGGammaValue; blueGamma: CGGammaValue ): CGDisplayErr; external name '_CGSetDisplayTransferByFormula';
                                              
function CGGetDisplayTransferByFormula( display: CGDirectDisplayID; var redMin: CGGammaValue; var redMax: CGGammaValue; var redGamma: CGGammaValue; var greenMin: CGGammaValue; var greenMax: CGGammaValue; var greenGamma: CGGammaValue; var blueMin: CGGammaValue; var blueMax: CGGammaValue; var blueGamma: CGGammaValue ): CGDisplayErr; external name '_CGGetDisplayTransferByFormula';
{
 * Returns the capacity, or nunber of entries, in the camma table for the specified
 * display.  If 'display' is invalid, returns 0.
 }
function CGDisplayGammaTableCapacity( display: CGDirectDisplayID ): CGTableCount; external name '_CGDisplayGammaTableCapacity'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{
 * Set a display gamma/transfer function using tables of data for each channel.
 * Values within each table should have values in the range of 0.0 through 1.0.
 * The same table may be passed in for red, green, and blue channels. 'tableSize'
 * indicates the number of entries in each table.
 * The tables are interpolated as needed to generate the number of samples needed
 * by hardware.
 }
function CGSetDisplayTransferByTable( display: CGDirectDisplayID; tableSize: CGTableCount; redTable: {const} CGGammaValuePtr; greenTable: {const} CGGammaValuePtr; blueTable: {const} CGGammaValuePtr ): CGDisplayErr; external name '_CGSetDisplayTransferByTable';

{
 * Get transfer tables.  Capacity should contain the number of samples each
 * array can hold, and *sampleCount is filled in with the number of samples
 * actually copied in.
 }
function CGGetDisplayTransferByTable( display: CGDirectDisplayID; capacity: CGTableCount; redTable: CGGammaValuePtr; greenTable: CGGammaValuePtr; blueTable: CGGammaValuePtr; var sampleCount: CGTableCount ): CGDisplayErr; external name '_CGGetDisplayTransferByTable';

{ As a convenience, allow setting of the gamma table by byte values }
function CGSetDisplayTransferByByteTable( display: CGDirectDisplayID; tableSize: CGTableCount; redTable: {const} CGByteValuePtr; greenTable: {const} CGByteValuePtr; blueTable: {const} CGByteValuePtr ): CGDisplayErr; external name '_CGSetDisplayTransferByByteTable';

{ Restore gamma tables of system displays to the user's ColorSync specified values }
procedure CGDisplayRestoreColorSyncSettings; external name '_CGDisplayRestoreColorSyncSettings';

{
 * Options used with CGDisplayCaptureWithOptions and CGCaptureAllDisplaysWithOptions
 }
const
	kCGCaptureNoOptions = 0;	{ Default behavior }
	kCGCaptureNoFill = 1 shl 0;	{ Disables fill with black on display capture }

{ Display capture and release }
function CGDisplayIsCaptured( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsCaptured';
function CGDisplayCapture( display: CGDirectDisplayID ): CGDisplayErr; external name '_CGDisplayCapture';
function CGDisplayCaptureWithOptions( display: CGDirectDisplayID; options: CGCaptureOptions ): CGDisplayErr; external name '_CGDisplayCaptureWithOptions'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
function CGDisplayRelease( display: CGDirectDisplayID ): CGDisplayErr; external name '_CGDisplayRelease';

{
 * Capture all displays; this has the nice effect of providing an immersive
 * environment, and preventing other apps from trying to adjust themselves
 * to display changes only needed by your app.
 }
function CGCaptureAllDisplays: CGDisplayErr; external name '_CGCaptureAllDisplays';
function CGCaptureAllDisplaysWithOptions( options: CGCaptureOptions ): CGDisplayErr; external name '_CGCaptureAllDisplaysWithOptions'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{
 * Release all captured displays, and restore the display modes to the
 * user's preferences.  May be used in conjunction with CGDisplayCapture()
 * or CGCaptureAllDisplays().
 }
function CGReleaseAllDisplays: CGDisplayErr; external name '_CGReleaseAllDisplays';

{
 * Returns CoreGraphics raw shield window ID or NULL if not shielded
 * This value may be used with drawing surface APIs.
 }
function CGShieldingWindowID( display: CGDirectDisplayID ): UnivPtr; external name '_CGShieldingWindowID';

{
 * Returns the window level used for the shield window.
 * This value may be used with Cocoa windows to position the
 * Cocoa window in the same window level as the shield window.
 }
function CGShieldingWindowLevel: SInt32; external name '_CGShieldingWindowLevel';

{
 * Returns base address of display or NULL for an invalid display.
 * If the display has not been captured, the returned address may refer
 * to read-only memory.
 }
function CGDisplayBaseAddress( display: CGDirectDisplayID ): UnivPtr; external name '_CGDisplayBaseAddress';
{
 * return address for X,Y in global coordinates;
 *	(0,0) represents the upper left corner of the main display.
 * returns NULL for an invalid display or out of bounds coordinates
 * If the display has not been captured, the returned address may refer
 * to read-only memory.
 }
function CGDisplayAddressForPosition( display: CGDirectDisplayID; x: CGDisplayCoord; y: CGDisplayCoord ): UnivPtr; external name '_CGDisplayAddressForPosition';


{ Mouse Cursor controls }
function CGDisplayHideCursor( display: CGDirectDisplayID ): CGDisplayErr; external name '_CGDisplayHideCursor';	{ increments hide cursor count }
function CGDisplayShowCursor( display: CGDirectDisplayID ): CGDisplayErr; external name '_CGDisplayShowCursor';	{ decrements hide cursor count  }

{
 * Move the cursor to the specified point relative to the display origin
 * (the upper left corner of the display).  Returns CGDisplayNoErr on success.
 * No events are generated as a result of this move.
 * Points that would lie outside the desktop are clipped to the desktop.
 }
function CGDisplayMoveCursorToPoint( display: CGDirectDisplayID; point: CGPoint ): CGDisplayErr; external name '_CGDisplayMoveCursorToPoint';

{
 * Report the mouse position change associated with the last mouse move event
 * recieved by this application.
 }
procedure CGGetLastMouseDelta( var deltaX: CGMouseDelta; var deltaY: CGMouseDelta ); external name '_CGGetLastMouseDelta';


{ Palette controls (8 bit pseudocolor only) }

{
 * Returns TRUE if the current display mode supports palettes.
 * Display must not be a hardware mirror of another, and should
 * have a depth of 8 bits per pixel for this to return TRUE.
 }
function CGDisplayCanSetPalette( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayCanSetPalette';

{
 * Set a palette.  The current gamma function is applied to the palette
 * elements before being loaded into hardware.  The display must not be
 * a hardware mirror of another, and should have a depth of 8 bits per pixel.
 * Setting the palette on the active, or primary display in a hardware
 * mirroring set affects all displays in that set.
 }
function CGDisplaySetPalette( display: CGDirectDisplayID; palette: CGDirectPaletteRef ): CGDisplayErr; external name '_CGDisplaySetPalette';

{
 * Wait until the beam position is outside the range specified by upperScanLine and lowerScanLine.
 * Note that if upperScanLine and lowerScanLine encompass the entire display height,
 * the function returns an error.
 * lowerScanLine must be greater than or equal to upperScanLine.
 *
 * Some display systems may not conventional video vertical and horizontal sweep in painting.
 * These displays report a kCGDisplayRefreshRate of 0 in the CFDictionaryRef returned by
 * CGDisplayCurrentMode().  On such displays, this function returns at once.
 *
 * Some drivers may not implement support for this mechanism.
 * On such displays, this function returns at once.
 *
 * Returns CGDisplayNoErr on success, and an error if display or upperScanLine and
 * lowerScanLine are invalid.
 *
 * The app should set the values of upperScanLine and lowerScanLine to allow enough lead time
 * for the drawing operation to complete.  A common strategy is to wait for the beam to pass
 * the bottom of the drawing area, allowing almost a full vertical sweep period to perform drawing.
 * To do this, set upperScanLine to 0, and set lowerScanLine to the bottom of the bounding box:
 *	lowerScanLine = (CGBeamPosition)(cgrect.origin.y + cgrect.size.height);
 *
 * IOKit may implement this as a spin-loop on the beam position call used for CGDisplayBeamPosition().
 * On such system the function is CPU bound, and subject to all the usual scheduling pre-emption.
 * In particular, attempting to wait for the beam to hit a specific scanline may be an exercise in frustration.
 *
 * These functions are advisary in nature, and depend on IOKit and hardware specific drivers to implement
 * support. If you need extremely precise timing, or access to vertical blanking interrupts,
 * you should consider writing a device driver to tie into hardware-specific capabilities.
 }
function CGDisplayWaitForBeamPositionOutsideLines( display: CGDirectDisplayID; upperScanLine: CGBeamPosition; lowerScanLine: CGBeamPosition ): CGDisplayErr; external name '_CGDisplayWaitForBeamPositionOutsideLines';

{
 * Returns the current beam position on the display.  If display is invalid,
 * or the display does not implement conventional video vertical and horizontal
 * sweep in painting, or the driver does not implement this functionality, 0 is returned.
 }
function CGDisplayBeamPosition( display: CGDirectDisplayID ): CGBeamPosition; external name '_CGDisplayBeamPosition';

{
 * Obtain a CGContextRef suitable for drawing to a captured display.
 *
 * Returns a drawing context suitable for use on the display device.
 * The context is owned by the device, and should not be released by
 * the caller.
 *
 * The context remains valid while the display is captured, and the
 * display configuration is unchanged.  Releasing the captured display
 * or reconfiguring the display invalidates the drawing context.
 *
 * An application may register a display reconfiguration callback to determine
 * when the display configuration is changing via CGRegisterDisplayReconfigurationProc().
 * 
 * After a display configuration change, or on capturing a display, call this
 * function to obtain a current drawing context.
 *
 * If the display has not been captured, this function returns NULL.
 }
function CGDisplayGetDrawingContext( display: CGDirectDisplayID ): CGContextRef; external name '_CGDisplayGetDrawingContext'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.

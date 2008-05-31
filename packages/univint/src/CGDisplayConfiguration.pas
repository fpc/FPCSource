{
 *  CGDisplayConfiguration.h
 *  CoreGraphics
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

unit CGDisplayConfiguration;
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
uses MacTypes,CGBase,CGDirectDisplay,CGErrors,CFDictionary,CGGeometry;
{$ALIGN POWER}


{
 * Display reconfiguration process.
 * Call CGBeginDisplayConfiguration to start.
 * Make all desired changes, for all displays.
 * Commit the changes using CGPerformDisplayConfiguration(), or cancel with
 * CGCancelDisplayConfiguration()
 *
 * The resulting layout will be adjusted so as to remove gaps or overlaps from
 * the requested layout, if needed.
 }
type
	CGDisplayConfigRef = ^SInt32; { an opaque 32-bit type }

{ Get a new CGDisplayConfigRef }
function CGBeginDisplayConfiguration( var pConfigRef: CGDisplayConfigRef ): CGError; external name '_CGBeginDisplayConfiguration';

{
 * Set the origin point for a display
 *
 * Note that setting the origin of a display which is mirroring
 * another display will remove that display from any mirroring set.
 *
 * Any display whose origin is not explicitly set in a reconfiguration
 * will be repositioned to a location as close as possible to it's
 * current location without overlapping or leaving a gap between displays.
 *
 * The actual position a display is placed at will be as close as possible
 * to the requested location without overlapping or leaving a gap between
 * displays.
 }
function CGConfigureDisplayOrigin( configRef: CGDisplayConfigRef; display: CGDirectDisplayID; x: CGDisplayCoord; y: CGDisplayCoord ): CGError; external name '_CGConfigureDisplayOrigin';

{
 * Set the display mode
 *
 * The mode dictionary passed in must be a dictionary vended by other CGDirectDisplay
 * APIs such as CGDisplayBestModeForParameters() and CGDisplayAvailableModes().
 *
 * When changing display modes of displays in a mirroring set, other displays in
 * the mirroring set whose mode is not explicitly changed will be set to a display
 * mode capable of mirroring the bounds of the largest display being explicitly set. 
 }
function CGConfigureDisplayMode( configRef: CGDisplayConfigRef; display: CGDirectDisplayID; mode: CFDictionaryRef ): CGError; external name '_CGConfigureDisplayMode';

{
 * Make a display a mirror of masterDisplay.
 *
 * Use a CGDirectDisplayID of kCGNullDirectDisplay for the masterDisplay to disable
 * mirroring.
 * Use a CGDirectDisplayID of CGMainDisplayID() for the masterDisplay to mirror
 * the main display.
 *
 * Mirroring requests will be filled with hardware mirroring when possible,
 * at the device driver's choice.  Displays will be matted as appropriate,
 * using either hardware or software matte generation, again at the device driver's choice.
 *
 * Note that when hardware mirroring is in effect, the device driver may bind the hardware
 * accelerator, drawing engine, and 3D engine to any one of the displays in the hardware
 * mirroring set.  That display will become the active display for drawing purposes in that
 * hardware mirroring set.  Use CGDisplayPrimaryDisplay() to determine the correct display
 * device to process drawing operations in a hardware mirroring set.
 *
 * An app that uses CGGetActiveDisplayList() to determine the proper displays to draw to
 * (All Carbon and Cocoa apps using windows and/or DrawSprocket fall into this class)
 * will automatically get the correct behavior.
 }
function CGConfigureDisplayMirrorOfDisplay( configRef: CGDisplayConfigRef; display: CGDirectDisplayID; masterDisplay: CGDirectDisplayID ): CGError; external name '_CGConfigureDisplayMirrorOfDisplay';

{ Cancel a reconfiguration operation, discarding the configRef }
function CGCancelDisplayConfiguration( configRef: CGDisplayConfigRef ): CGError; external name '_CGCancelDisplayConfiguration';

{
 * Perform the requested reconfigurations and discard the configRef
 *
 * A configuration change can apply for the life of an app, the life of a login session, or
 * permanently. If a request is made to make a change permanent, and the change
 * cannot be supported by the Aqua UI (resolution and pixel depth constraints apply),
 * then the configuration  change is demoted to lasting the session.
 *
 * A permanent configuration change also becomes the current session's
 * configuration.
 *
 * When the system reverts confgurations at app termination, the
 * configuration always reverts to the session or permanent configuration setting.
 *
 * When the system reverts confgurations at session termination, the
 * configuration always reverts to the permanent configuration setting.
 *
 * This operation may fail if:
 *     An unsupported display mode is requested
 *     Another app is running in full-screen mode
 *
 }
const
	kCGConfigureForAppOnly = 0;
	kCGConfigureForSession = 1;
	kCGConfigurePermanently = 2;
type
	CGConfigureOption = UInt32;

function CGCompleteDisplayConfiguration( configRef: CGDisplayConfigRef; option: CGConfigureOption ): CGError; external name '_CGCompleteDisplayConfiguration';

{ Restore the permanent display configuration from the user's display preferences settings }
procedure CGRestorePermanentDisplayConfiguration; external name '_CGRestorePermanentDisplayConfiguration';

{
 * Applications may want to register for notifications of display changes.
 *
 * Display changes are reported via a callback mechanism.
 *
 * Callbacks are invoked when the app is listening for events,
 * on the event processing thread, or from within the display
 * reconfiguration function when in the program that is driving the
 * reconfiguration.
 *
 * Callbacks should avoid attempting to change display configurations,
 * and should not raise exceptions or perform a non-local return such as
 * calling longjmp().
 *
 * Before display reconfiguration, a callback fires to inform
 * applications of a pending configuration change. The callback runs
 * once for each on-line display.  The flags passed in are set to
 * kCGDisplayBeginConfigurationFlag.  This callback does not
 * carry other per-display information, as details of how a
 * reconfiguration affects a particular device rely on device-specific
 * behaviors which may not be exposed by a device driver.
 *
 * After display reconfiguration, at the time the callback function
 * is invoked, all display state reported by CoreGraphics, QuickDraw,
 * and the Carbon Display Manager API will be up to date.  This callback
 * runs after the Carbon Display Manager notification callbacks.
 * The callback runs once for each added, removed, and currently
 * on-line display.  Note that in the case of removed displays, calls into
 * the CoreGraphics API with the removed display ID will fail.
 }

const
	kCGDisplayBeginConfigurationFlag = 1 shl 0; { Set in pre-reconfiguration callback }
	kCGDisplayMovedFlag = 1 shl 1; { post-reconfiguration callback flag }
	kCGDisplaySetMainFlag = 1 shl 2; { post-reconfiguration callback flag }
	kCGDisplaySetModeFlag = 1 shl 3; { post-reconfiguration callback flag }
	kCGDisplayAddFlag = 1 shl 4; { post-reconfiguration callback flag }
	kCGDisplayRemoveFlag = 1 shl 5; { post-reconfiguration callback flag }
	kCGDisplayEnabledFlag = 1 shl 8; { post-reconfiguration callback flag }
	kCGDisplayDisabledFlag = 1 shl 9; { post-reconfiguration callback flag }
	kCGDisplayMirrorFlag = 1 shl 10;{ post-reconfiguration callback flag }
	kCGDisplayUnMirrorFlag = 1 shl 11; { post-reconfiguration callback flag }
type
	CGDisplayChangeSummaryFlags = UInt32;

type
	CGDisplayReconfigurationCallBack = procedure( display: CGDirectDisplayID; flags: CGDisplayChangeSummaryFlags; userInfo: UnivPtr );

{
 * Register and remove a display reconfiguration callback procedure
 * The userInfo argument is passed back to the callback procedure each time
 * it is invoked.
 }
function CGDisplayRegisterReconfigurationCallback( proc: CGDisplayReconfigurationCallBack; userInfo: UnivPtr ): CGError; external name '_CGDisplayRegisterReconfigurationCallback'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
function CGDisplayRemoveReconfigurationCallback( proc: CGDisplayReconfigurationCallBack; userInfo: UnivPtr ): CGError; external name '_CGDisplayRemoveReconfigurationCallback'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{
 * These APIs allow applications and higher level frameworks
 * such as DrawSprocket to determine interesting properties
 * of displays, such as if a display is built-in, if a display
 * is the main display, if a display is being mirrored, which
 * display in a hardware mirror set is bound to the graphics
 * accelerator (important for games!) and so on.
 *
 * An app that uses CGGetActiveDisplayList() to determine the
 * proper displays to draw to (All Carbon and Cocoa apps using
 * windows and/or DrawSprocket fall into this class) will
 * automatically get the correct behavior without using these APIs.
 * These APIs are primarily of interest to specialized applications
 * such as movie players, integrated TV/video graphics utilities,
 * and similar specialized applications.
 }

{ True if the display is connected, awake, and drawable }
function CGDisplayIsActive( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsActive';

{ True if the display is asleep and therefore not drawable }
function CGDisplayIsAsleep( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsAsleep';

{
 * True if the display is valid, with a monitor connected
 * (support for hot plugging of monitors)
 }
function CGDisplayIsOnline( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsOnline';

{ True if the display is the current main display }
function CGDisplayIsMain( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsMain';

{ True if the display is built in, such as the internal display in portables }
function CGDisplayIsBuiltin( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsBuiltin';

{ True if the display is in a mirroring set }
function CGDisplayIsInMirrorSet( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsInMirrorSet';

{ True if the display is always in a mirroring set, and cannot be unmirrored }
function CGDisplayIsAlwaysInMirrorSet( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsAlwaysInMirrorSet';

{ True if the display is in a hardware mirroring set }
function CGDisplayIsInHWMirrorSet( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayIsInHWMirrorSet';

{ Returns display being mirrored, or kCGNullDirectDisplay if master or unmirrored }
function CGDisplayMirrorsDisplay( display: CGDirectDisplayID ): CGDirectDisplayID; external name '_CGDisplayMirrorsDisplay';

{ True if the display is using OpenGL acceleration }
function CGDisplayUsesOpenGLAcceleration( display: CGDirectDisplayID ): boolean_t; external name '_CGDisplayUsesOpenGLAcceleration';


{
 * Returns the display bound to the hardware accelerator in a HW mirror set,
 * or 'display' if software mirrored or unmirrored
 }
function CGDisplayPrimaryDisplay( display: CGDirectDisplayID ): CGDirectDisplayID; external name '_CGDisplayPrimaryDisplay';

{
 * Returns the logical unit, vendor ID, vendor model number,
 * and serial number for a display
 }
function CGDisplayUnitNumber( display: CGDirectDisplayID ): UInt32; external name '_CGDisplayUnitNumber';
function CGDisplayVendorNumber( display: CGDirectDisplayID ): UInt32; external name '_CGDisplayVendorNumber';
function CGDisplayModelNumber( display: CGDirectDisplayID ): UInt32; external name '_CGDisplayModelNumber';
function CGDisplaySerialNumber( display: CGDirectDisplayID ): UInt32; external name '_CGDisplaySerialNumber';

{ Returns the IOKit service port for a display device }
// uncomment when IOKit translated function CGDisplayIOServicePort( display: CGDirectDisplayID ): io_service_t;

{
 * Returns the size of the specified display in millimeters.
 *
 * If 'display' is not a valid display ID, the size returned has a width and height of 0.
 *
 * If EDID data for the display device is not available, the size is estimated based on
 * the device width and height in pixels from CGDisplayBounds(), with an assumed resolution
 * of 2.835 pixels/mm, or 72 DPI, a reasonable guess for displays predating EDID support.
 }
function CGDisplayScreenSize( display: CGDirectDisplayID ): CGSize; external name '_CGDisplayScreenSize'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.

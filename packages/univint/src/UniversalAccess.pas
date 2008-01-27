{
     File:       HIServices/UniversalAccess.h
 
     Contains:   Universal Access Interfaces.
 
     Version:    HIServices-169~377
 
     Copyright:  © 2005-2006 by Apple Computer, Inc., all rights reserved.
 
}

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit UniversalAccess;
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
uses MacTypes, CGGeometry;
{$ALIGN POWER}


{
 *  Universal Access
 *  
 *  Discussion:
 *    Universal Access provides apps the ability to control the zoom
 *    focus. The following functions are provided so that applications
 *    can tell Universal Access what part of the UI needs focus.
 }

{
 *  UAZoomChangeFocusType
 *  
 *  Summary:
 *    Universal Access Zoom Change Focus Types
 *  
 *  Discussion:
 *    The following constants are used to tell Universal Access Zoom
 *    the type of event that is driving the change in the zoom focus.
 }
type
	UAZoomChangeFocusType = UInt32;
const
{
   * Some event would like focus.
   }
	kUAZoomFocusTypeOther = 0;

  {
   * The text insertion point has moved.
   }
	kUAZoomFocusTypeInsertionPoint = 1;


{
 *  UAZoomEnabled()
 *  
 *  Summary:
 *    Determine if Universal Access Zoom is enabled.
 *  
 *  Discussion:
 *    This queries the state of Universal Access Zoom
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Result:
 *    TRUE if Universal Access Zoom is on, FALSE if Zoom is off or the
 *    user has zoomed all the way out.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function UAZoomEnabled: Boolean; external name '_UAZoomEnabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  UAZoomChangeFocus()
 *  
 *  Summary:
 *    Tell Universal Access where Zoom should focus.
 *  
 *  Discussion:
 *    Tells Universal Access the frame of the element in focus and the
 *    part of the element that should be in focus.
 *  
 *  Mac OS X threading:
 *    Thread safe
 *  
 *  Parameters:
 *    
 *    inRect:
 *      - The frame of the element in focus in global 72dpi coordinates.
 *    
 *    inHighlightRect:
 *      - The frame of highlighted part of the element in focus in
 *      global 72dpi coordinates.  If the whole element is in focus,
 *      and not just a smaller part of it, pass the inRect parameter
 *      and pass NULL for inHighlightRect.
 *    
 *    inType:
 *      - Universal Access Zoom change focus type.
 *  
 *  Result:
 *    OSStatus - noErr if there were no problems or Universal Access
 *    Zoom is off or zoomed all the way out.  paramErr if inRect is
 *    NULL or inType is out of range.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function UAZoomChangeFocus( const (*var*) inRect: CGRect; inHighlightRect: CGRectPtr; inType: UAZoomChangeFocusType ): OSStatus; external name '_UAZoomChangeFocus';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.

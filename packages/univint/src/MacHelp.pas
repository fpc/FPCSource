{
     File:       HIToolbox/MacHelp.h
 
     Contains:   Macintosh Help Package Interfaces.
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 1998-2005 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
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

unit MacHelp;
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
uses MacTypes,CFBase,Quickdraw,TextEdit,Controls,Dialogs,Events,MacWindows,Menus;


{$ALIGN MAC68K}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Help Manager constants, etc.                                                     }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kMacHelpVersion = $0003;

const
	kHMHelpMenuID = -16490; { Resource ID and menu ID of help menu }

type
	HMContentRequest = SInt16;
const
	kHMSupplyContent = 0;
	kHMDisposeContent = 1;

type
	HMContentType = UInt32;
const
	kHMNoContent = $6E6F6E65 (* 'none' *);
	kHMCFStringContent = $63667374 (* 'cfst' *); { CFStringRef}
	kHMCFStringLocalizedContent = $6366736C (* 'cfsl' *); { CFStringRef; name of a localized string. Supported by Jaguar and later.}
	kHMPascalStrContent = $70737472 (* 'pstr' *);
	kHMStringResContent = $73747223 (* 'str#' *);
	kHMTEHandleContent = $74787468 (* 'txth' *); { Supported by CarbonLib and Jaguar and later}
	kHMTextResContent = $74657874 (* 'text' *); { Supported by CarbonLib and Jaguar and later}
	kHMStrResContent = $73747220 (* 'str ' *);


{
 *  HMTagDisplaySide
 *  
 *  Discussion:
 *    Help tag display locations relative to absolute hot rect
 }
type
	HMTagDisplaySide = SInt16;
const
{
   * System default location
   }
	kHMDefaultSide = 0;

  {
   * Above, aligned with left or right depending on system script
   }
	kHMOutsideTopScriptAligned = 1;

  {
   * To the left, centered vertically
   }
	kHMOutsideLeftCenterAligned = 2;
	kHMOutsideBottomScriptAligned = 3;

  {
   * To the right, centered vertically
   }
	kHMOutsideRightCenterAligned = 4;

  {
   * Above, aligned with left
   }
	kHMOutsideTopLeftAligned = 5;

  {
   * Above, aligned with right
   }
	kHMOutsideTopRightAligned = 6;

  {
   * To the left, aligned with top
   }
	kHMOutsideLeftTopAligned = 7;

  {
   * To the left, aligned with bottom
   }
	kHMOutsideLeftBottomAligned = 8;

  {
   * To the right, aligned with top
   }
	kHMOutsideBottomLeftAligned = 9;

  {
   * To the right, aligned with bottom
   }
	kHMOutsideBottomRightAligned = 10;
	kHMOutsideRightTopAligned = 11;
	kHMOutsideRightBottomAligned = 12;

  {
   * Above, centered horizontally
   }
	kHMOutsideTopCenterAligned = 13;

  {
   * Below, centered horizontally
   }
	kHMOutsideBottomCenterAligned = 14;

  {
   * Inside, aligned with right, centered vertically
   }
	kHMInsideRightCenterAligned = 15;

  {
   * Inside, aligned with left, centered vertically
   }
	kHMInsideLeftCenterAligned = 16;

  {
   * Inside, aligned with bottom, centered horizontally
   }
	kHMInsideBottomCenterAligned = 17;

  {
   * Inside, aligned with top, centered horizontally
   }
	kHMInsideTopCenterAligned = 18;

  {
   * Inside, aligned with top and left
   }
	kHMInsideTopLeftCorner = 19;

  {
   * Inside, aligned with top and right
   }
	kHMInsideTopRightCorner = 20;

  {
   * Inside, aligned with bottom and left
   }
	kHMInsideBottomLeftCorner = 21;

  {
   * Inside, aligned with bottom and right
   }
	kHMInsideBottomRightCorner = 22;

  {
   * Centered vertically and horizontally
   }
	kHMAbsoluteCenterAligned = 23;

{ Obsoleted constants HMTagDisplaySides, use the new ones, please }
const
	kHMTopSide = kHMOutsideTopScriptAligned;
	kHMLeftSide = kHMOutsideLeftCenterAligned;
	kHMBottomSide = kHMOutsideBottomScriptAligned;
	kHMRightSide = kHMOutsideRightCenterAligned;
	kHMTopLeftCorner = kHMOutsideTopLeftAligned;
	kHMTopRightCorner = kHMOutsideTopRightAligned;
	kHMLeftTopCorner = kHMOutsideLeftTopAligned;
	kHMLeftBottomCorner = kHMOutsideLeftBottomAligned;
	kHMBottomLeftCorner = kHMOutsideBottomLeftAligned;
	kHMBottomRightCorner = kHMOutsideBottomRightAligned;
	kHMRightTopCorner = kHMOutsideRightTopAligned;
	kHMRightBottomCorner = kHMOutsideRightBottomAligned;

type
	HMContentProvidedType = SInt16;
const
	kHMContentProvided = 0;
	kHMContentNotProvided = 1;
	kHMContentNotProvidedDontPropagate = 2;

const
	kHMMinimumContentIndex = 0;    { first entry in HMHelpContentRec.content is the minimum content }
	kHMMaximumContentIndex = 1;     { second entry in HMHelpContentRec.content is the maximum content }

const
	errHMIllegalContentForMinimumState = -10980; { unrecognized content type for minimum content }
	errHMIllegalContentForMaximumState = -10981; { unrecognized content type for maximum content }

{ obsolete names; will be removed}
const
	kHMIllegalContentForMinimumState = errHMIllegalContentForMinimumState;

const
	kHelpTagEventHandlerTag = $68657674 (* 'hevt' *);

type
	HMStringResType = record
		hmmResID: SInt16;
		hmmIndex: SInt16;
	end;
type
	HMHelpContent = record
		contentType: HMContentType;
		case SInt16 of
		0: (
			tagCFString:		CFStringRef;							{  CFStringRef }
			);
		1: (
			tagString:			Str255;									{  Pascal String }
			);
		2: (
			tagStringRes:		HMStringResType;						{  STR# resource ID and index }
			);
		3: (
			tagTEHandle:		TEHandle;								{  TextEdit handle (NOT SUPPORTED ON MAC OS X) }
			);
		4: (
			tagTextRes:			SInt16;									{  TEXT/styl resource ID (NOT SUPPORTED ON MAC OS X) }
			);
		5: (
			tagStrRes:			SInt16;									{  STR resource ID }
			);
	end;
type
	HMHelpContentRec = record
		version: SInt32;
		absHotRect: Rect;
		tagSide: HMTagDisplaySide;
		content: array [0..1] of HMHelpContent;
	end;
	HMHelpContentPtr = ^HMHelpContentRec;

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Callback procs                                       }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ }
type
	HMControlContentProcPtr = function( inControl: ControlRef; inGlobalMouse: Point; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr ): OSStatus;
type
	HMWindowContentProcPtr = function( inWindow: WindowRef; inGlobalMouse: Point; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr ): OSStatus;
type
	HMMenuTitleContentProcPtr = function( inMenu: MenuRef; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr ): OSStatus;
type
	HMMenuItemContentProcPtr = function( const (*var*) inTrackingData: MenuTrackingData; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr ): OSStatus;
type
	HMControlContentUPP = HMControlContentProcPtr;
type
	HMWindowContentUPP = HMWindowContentProcPtr;
type
	HMMenuTitleContentUPP = HMMenuTitleContentProcPtr;
type
	HMMenuItemContentUPP = HMMenuItemContentProcPtr;
{
 *  NewHMControlContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewHMControlContentUPP( userRoutine: HMControlContentProcPtr ): HMControlContentUPP; external name '_NewHMControlContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewHMWindowContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewHMWindowContentUPP( userRoutine: HMWindowContentProcPtr ): HMWindowContentUPP; external name '_NewHMWindowContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewHMMenuTitleContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewHMMenuTitleContentUPP( userRoutine: HMMenuTitleContentProcPtr ): HMMenuTitleContentUPP; external name '_NewHMMenuTitleContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewHMMenuItemContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewHMMenuItemContentUPP( userRoutine: HMMenuItemContentProcPtr ): HMMenuItemContentUPP; external name '_NewHMMenuItemContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeHMControlContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeHMControlContentUPP( userUPP: HMControlContentUPP ); external name '_DisposeHMControlContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeHMWindowContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeHMWindowContentUPP( userUPP: HMWindowContentUPP ); external name '_DisposeHMWindowContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeHMMenuTitleContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeHMMenuTitleContentUPP( userUPP: HMMenuTitleContentUPP ); external name '_DisposeHMMenuTitleContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeHMMenuItemContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeHMMenuItemContentUPP( userUPP: HMMenuItemContentUPP ); external name '_DisposeHMMenuItemContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeHMControlContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeHMControlContentUPP( inControl: ControlRef; inGlobalMouse: Point; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr; userUPP: HMControlContentUPP ): OSStatus; external name '_InvokeHMControlContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeHMWindowContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeHMWindowContentUPP( inWindow: WindowRef; inGlobalMouse: Point; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr; userUPP: HMWindowContentUPP ): OSStatus; external name '_InvokeHMWindowContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeHMMenuTitleContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeHMMenuTitleContentUPP( inMenu: MenuRef; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr; userUPP: HMMenuTitleContentUPP ): OSStatus; external name '_InvokeHMMenuTitleContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeHMMenuItemContentUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeHMMenuItemContentUPP( const (*var*) inTrackingData: MenuTrackingData; inRequest: HMContentRequest; var outContentProvided: HMContentProvidedType; ioHelpContent: HMHelpContentPtr; userUPP: HMMenuItemContentUPP ): OSStatus; external name '_InvokeHMMenuItemContentUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ API                                                                                      }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Help Menu }
{
 *  HMGetHelpMenu()
 *  
 *  Summary:
 *    Returns a menu to which applications may add their own help items.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    outHelpMenu:
 *      On exit, contains the help menu.
 *    
 *    outFirstCustomItemIndex:
 *      On exit, contains the menu item index that will be used by the
 *      first item added by the application. This parameter may be NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetHelpMenu( var outHelpMenu: MenuRef; outFirstCustomItemIndex: MenuItemIndexPtr { can be NULL } ): OSStatus; external name '_HMGetHelpMenu';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Installing/Retrieving Content }
{ Menu title and item help tags are not supported by CarbonLib. They are fully supported on Mac OS X. }
{ Pass NULL for the inContent parameter of HMSetControl/Window/MenuItemHelpContent to remove help content
       from a control, window, or menu. }
{
 *  HMSetControlHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetControlHelpContent( inControl: ControlRef; {const} inContent: HMHelpContentPtr { can be NULL } ): OSStatus; external name '_HMSetControlHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetControlHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetControlHelpContent( inControl: ControlRef; var outContent: HMHelpContentRec ): OSStatus; external name '_HMGetControlHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMSetWindowHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetWindowHelpContent( inWindow: WindowRef; {const} inContent: HMHelpContentPtr { can be NULL } ): OSStatus; external name '_HMSetWindowHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetWindowHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetWindowHelpContent( inWindow: WindowRef; var outContent: HMHelpContentRec ): OSStatus; external name '_HMGetWindowHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMSetMenuItemHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetMenuItemHelpContent( inMenu: MenuRef; inItem: MenuItemIndex; {const} inContent: HMHelpContentPtr { can be NULL } ): OSStatus; external name '_HMSetMenuItemHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetMenuItemHelpContent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetMenuItemHelpContent( inMenu: MenuRef; inItem: MenuItemIndex; var outContent: HMHelpContentRec ): OSStatus; external name '_HMGetMenuItemHelpContent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Installing/Retrieving Content Callbacks }
{
 *  HMInstallControlContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMInstallControlContentCallback( inControl: ControlRef; inContentUPP: HMControlContentUPP ): OSStatus; external name '_HMInstallControlContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMInstallWindowContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMInstallWindowContentCallback( inWindow: WindowRef; inContentUPP: HMWindowContentUPP ): OSStatus; external name '_HMInstallWindowContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMInstallMenuTitleContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMInstallMenuTitleContentCallback( inMenu: MenuRef; inContentUPP: HMMenuTitleContentUPP ): OSStatus; external name '_HMInstallMenuTitleContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMInstallMenuItemContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMInstallMenuItemContentCallback( inMenu: MenuRef; inContentUPP: HMMenuItemContentUPP ): OSStatus; external name '_HMInstallMenuItemContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetControlContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetControlContentCallback( inControl: ControlRef; var outContentUPP: HMControlContentUPP ): OSStatus; external name '_HMGetControlContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetWindowContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetWindowContentCallback( inWindow: WindowRef; var outContentUPP: HMWindowContentUPP ): OSStatus; external name '_HMGetWindowContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetMenuTitleContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetMenuTitleContentCallback( inMenu: MenuRef; var outContentUPP: HMMenuTitleContentUPP ): OSStatus; external name '_HMGetMenuTitleContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetMenuItemContentCallback()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetMenuItemContentCallback( inMenu: MenuRef; var outContentUPP: HMMenuItemContentUPP ): OSStatus; external name '_HMGetMenuItemContentCallback';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Enabling and Disabling Help Tags }
{
 *  HMAreHelpTagsDisplayed()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMAreHelpTagsDisplayed: Boolean; external name '_HMAreHelpTagsDisplayed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMSetHelpTagsDisplayed()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetHelpTagsDisplayed( inDisplayTags: Boolean ): OSStatus; external name '_HMSetHelpTagsDisplayed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMSetTagDelay()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetTagDelay( inDelay: Duration ): OSStatus; external name '_HMSetTagDelay';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMGetTagDelay()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMGetTagDelay( var outDelay: Duration ): OSStatus; external name '_HMGetTagDelay';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Compatibility }
{
 *  HMSetMenuHelpFromBalloonRsrc()
 *  
 *  Summary:
 *    Not really implemented.
 *  
 *  Discussion:
 *    Though this API is exported from CarbonLib and Mac OS X, it is
 *    completely non-functional. We have no plans to implement it.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetMenuHelpFromBalloonRsrc( inMenu: MenuRef; inHmnuRsrcID: SInt16 ): OSStatus; external name '_HMSetMenuHelpFromBalloonRsrc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMSetDialogHelpFromBalloonRsrc()
 *  
 *  Summary:
 *    Not really implemented.
 *  
 *  Discussion:
 *    Though this API is exported from CarbonLib and Mac OS X, it is
 *    completely non-functional. We have no plans to implement it.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function HMSetDialogHelpFromBalloonRsrc( inDialog: DialogRef; inHdlgRsrcID: SInt16; inItemStart: SInt16 ): OSStatus; external name '_HMSetDialogHelpFromBalloonRsrc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Displaying tags }
{
 *  HMDisplayTag()
 *  
 *  Summary:
 *    Displays a help tag at a user-defined location.
 *  
 *  Discussion:
 *    Note that HMDisplayTag does not currently retain the help content
 *    that is passed to it, nor release it when the tag is closed. Your
 *    application must ensure that the help content remains valid as
 *    long as the tag may be visible (which effectively means that your
 *    application should never dispose of help content that is passed
 *    to HMDisplayTag).
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inContent:
 *      HMHelpContentRec describing the help tag to be displayed.
 *  
 *  Result:
 *    An OSStatus code indicating success or failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.2 and later
 *    Non-Carbon CFM:   not available
 }
function HMDisplayTag( const (*var*) inContent: HMHelpContentRec ): OSStatus; external name '_HMDisplayTag';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HMHideTag()
 *  
 *  Summary:
 *    HMHideTag hides the currently visible help tag.  If there is no
 *    current help tag, this call does nothing.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Result:
 *    An OSStatus code indicating success or failure.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.2 and later
 *    Non-Carbon CFM:   not available
 }
function HMHideTag: OSStatus; external name '_HMHideTag';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  Summary:
 *    Values for the inOptions parameter to HMHideTagWithOptions.
 }
const
{
   * Causes the tag to fade out when hidden. If this flag is not
   * specified, the tag is hidden without fading.
   }
	kHMHideTagFade = 1 shl 0;

  {
   * Causes the tag to begin hiding immediately. If this flag is not
   * specified, the tag is hidden after a short delay (currently 0.75
   * second).
   }
	kHMHideTagImmediately = 1 shl 1;

{
 *  HMHideTagWithOptions()
 *  
 *  Summary:
 *    Hides the current help tag, with various options to control how
 *    the tag is hidden.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options for how the tag should be hidden. kHMHideTagFade and
 *      kHMHideTagImmediately are the only available options.
 *  
 *  Result:
 *    An operating system result code. noErr is returned if there is no
 *    tag currently visible.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function HMHideTagWithOptions( inOptions: OptionBits ): OSStatus; external name '_HMHideTagWithOptions';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)




end.

{
 *  AXNotificationConstants.h
 *  HIServices
 *
 *  Created by John Louch on Wed Feb 25 2004.
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit AXNotificationConstants;
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
uses MacTypes;
{$ALIGN POWER}


// focus notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMainWindowChangedNotification CFSTRP('AXMainWindowChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedWindowChangedNotification CFSTRP('AXFocusedWindowChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedUIElementChangedNotification CFSTRP('AXFocusedUIElementChanged')}
{$endc}

// application notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationActivatedNotification CFSTRP('AXApplicationActivated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationDeactivatedNotification CFSTRP('AXApplicationDeactivated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationHiddenNotification CFSTRP('AXApplicationHidden')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationShownNotification CFSTRP('AXApplicationShown')}
{$endc}

// window notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowCreatedNotification CFSTRP('AXWindowCreated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowMovedNotification CFSTRP('AXWindowMoved')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowResizedNotification CFSTRP('AXWindowResized')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowMiniaturizedNotification CFSTRP('AXWindowMiniaturized')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowDeminiaturizedNotification CFSTRP('AXWindowDeminiaturized')}
{$endc}

// new drawer, sheet, and help tag notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDrawerCreatedNotification CFSTRP('AXDrawerCreated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSheetCreatedNotification CFSTRP('AXSheetCreated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHelpTagCreatedNotification CFSTRP('AXHelpTagCreated')}
{$endc}

// element notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXValueChangedNotification CFSTRP('AXValueChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXUIElementDestroyedNotification CFSTRP('AXUIElementDestroyed')}
{$endc}

// menu notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuOpenedNotification CFSTRP('AXMenuOpened')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuClosedNotification CFSTRP('AXMenuClosed')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemSelectedNotification CFSTRP('AXMenuItemSelected')}
{$endc}

// table/outline notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRowCountChangedNotification CFSTRP('AXRowCountChanged')}
{$endc}

// other notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedChildrenChangedNotification CFSTRP('AXSelectedChildrenChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXResizedNotification CFSTRP('AXResized')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMovedNotification CFSTRP('AXMoved')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCreatedNotification CFSTRP('AXCreated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedRowsChangedNotification CFSTRP('AXSelectedRowsChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedColumnsChangedNotification CFSTRP('AXSelectedColumnsChanged')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedTextChangedNotification CFSTRP('AXSelectedTextChanged')}
{$endc}


end.

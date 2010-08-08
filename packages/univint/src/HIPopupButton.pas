{
     File:       HIToolbox/HIPopupButton.h
 
     Contains:   Definitions of the popup button and popup arrow views provided by HIToolbox.
 
     Version:    HIToolbox-437~1
 
     Copyright:  © 2006-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit HIPopupButton;
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
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,Appearance,CarbonEvents,Controls,Menus,QuickdrawTypes,CFBase,HIObject;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{
 *  HIPopupButton.h
 *  
 *  Discussion:
 *    API definitions for the popup button and popup arrow views.
 }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ POPUP BUTTON (CDEF 25)                                                            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  This is the new Appearance Popup Button. It takes the same variants and does the    }
{  same overloading as the previous popup menu control. There are some differences:    }
{  Passing in a menu ID of -12345 causes the popup not to try and get the menu from a  }
{  resource. Instead, you can build the menu and later stuff the MenuRef field in      }
{  the popup data information.                                                         }
{  You can pass -1 in the Max parameter to have the control calculate the width of the }
{  title on its own instead of guessing and then tweaking to get it right. It adds the }
{  appropriate amount of space between the title and the popup.                        }
{ Theme Popup Button proc IDs }
const
	kControlPopupButtonProc = 400;
	kControlPopupFixedWidthVariant = 1 shl 0;
	kControlPopupVariableWidthVariant = 1 shl 1;
	kControlPopupUseAddResMenuVariant = 1 shl 2;
	kControlPopupUseWFontVariant = kControlUsesOwningWindowsFontVariant;

{ Control Kind Tag }
const
	kControlKindPopupButton = FourCharCode('popb');

{ The HIObject class ID for the HIPopupButton class. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kHIPopupButtonClassID CFSTRP('com.apple.HIPopupButton')}
{$endc}
{$ifc not TARGET_CPU_64}
{
 *  CreatePopupButtonControl()
 *  
 *  Summary:
 *    Creates a popup button control.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    window:
 *      The window that should contain the control. May be NULL on 10.3
 *      and later.
 *    
 *    boundsRect:
 *      The bounding box of the control.
 *    
 *    title:
 *      The title of the control.
 *    
 *    menuID:
 *      The ID of a menu that should be used by the control. A menu
 *      with this ID should be inserted into the menubar with
 *      InsertMenu(menu, kInsertHierarchicalMenu). You can also pass
 *      -12345 to have the control delay its acquisition of a menu; in
 *      this case, you can build the menu and later provide it to the
 *      control with SetControlData and kControlPopupButtonMenuRefTag
 *      or kControlPopupButtonOwnedMenuRefTag.
 *    
 *    variableWidth:
 *      Whether the width of the control is allowed to vary according
 *      to the width of the selected menu item text, or should remain
 *      fixed to the original control bounds width.
 *    
 *    titleWidth:
 *      The width of the title.
 *    
 *    titleJustification:
 *      The justification of the title.
 *    
 *    titleStyle:
 *      A QuickDraw style bitfield indicating the font style of the
 *      title.
 *    
 *    outControl:
 *      On exit, contains the new control.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreatePopupButtonControl( window: WindowRef { can be NULL }; const (*var*) boundsRect: Rect; title: CFStringRef; menuID_: MenuID; variableWidth: Boolean; titleWidth: SInt16; titleJustification: SInt16; titleStyle: Style; var outControl: ControlRef ): OSStatus; external name '_CreatePopupButtonControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ These tags are available in 1.0.1 or later of Appearance }
{$endc} {not TARGET_CPU_64}

const
	kControlPopupButtonMenuHandleTag = FourCharCode('mhan'); { MenuRef}
	kControlPopupButtonMenuRefTag = FourCharCode('mhan'); { MenuRef}
	kControlPopupButtonMenuIDTag = FourCharCode('mnid'); { SInt16}

{ These tags are available in 1.1 or later of Appearance }
const
	kControlPopupButtonExtraHeightTag = FourCharCode('exht'); { SInt16 - extra vertical whitespace within the button}
	kControlPopupButtonOwnedMenuRefTag = FourCharCode('omrf'); { MenuRef}

{ These tags are available in Mac OS X }
const
	kControlPopupButtonCheckCurrentTag = FourCharCode('chck'); { Boolean    - whether the popup puts a checkmark next to the current item (defaults to true)}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ POPUP ARROW (CDEF 12)                                                             }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  The popup arrow CDEF is used to draw the small arrow normally associated with a     }
{  popup control. The arrow can point in four directions, and a small or large version }
{  can be used. This control is provided to allow clients to draw the arrow in a       }
{  normalized fashion which will take advantage of themes automatically.               }
{ Popup Arrow proc IDs }
const
	kControlPopupArrowEastProc = 192;
	kControlPopupArrowWestProc = 193;
	kControlPopupArrowNorthProc = 194;
	kControlPopupArrowSouthProc = 195;
	kControlPopupArrowSmallEastProc = 196;
	kControlPopupArrowSmallWestProc = 197;
	kControlPopupArrowSmallNorthProc = 198;
	kControlPopupArrowSmallSouthProc = 199;

{ Popup Arrow Orientations }
const
	kControlPopupArrowOrientationEast = 0;
	kControlPopupArrowOrientationWest = 1;
	kControlPopupArrowOrientationNorth = 2;
	kControlPopupArrowOrientationSouth = 3;

type
	ControlPopupArrowOrientation = UInt16;
{ Popup Arrow Size }
const
	kControlPopupArrowSizeNormal = 0;
	kControlPopupArrowSizeSmall = 1;

type
	ControlPopupArrowSize = UInt16;
{ Control Kind Tag }
const
	kControlKindPopupArrow = FourCharCode('parr');

{ The HIObject class ID for the HIPopupArrow class. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kHIPopupArrowClassID CFSTRP('com.apple.hipopuparrow')}
{$endc}
{$ifc not TARGET_CPU_64}

{
 *  CreatePopupArrowControl()
 *  
 *  Summary:
 *    Creates a popup arrow control.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    window:
 *      The window that should contain the control. May be NULL on 10.3
 *      and later.
 *    
 *    boundsRect:
 *      The bounding box of the control.
 *    
 *    orientation:
 *      The orientation of the control.
 *    
 *    size:
 *      The size of the control.
 *    
 *    outControl:
 *      On exit, contains the new control.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreatePopupArrowControl( window: WindowRef { can be NULL }; const (*var*) boundsRect: Rect; orientation: ControlPopupArrowOrientation; size: ControlPopupArrowSize; var outControl: ControlRef ): OSStatus; external name '_CreatePopupArrowControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{--------------------------------------------------------------------------------------}
{  ¥ DEPRECATED                                                                        }
{  All declarations below this point are either deprecated (they continue to function  }
{  but are not the most modern nor most efficient solution to a problem), or they are  }
{  completely unavailable on Mac OS X.                                                 }
{--------------------------------------------------------------------------------------}
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ Pop-Up Menu Control Constants                                                     }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Variant codes for the System 7 pop-up menu}
{$endc} {not TARGET_CPU_64}

const
	popupFixedWidth = 1 shl 0;
	popupVariableWidth = 1 shl 1;
	popupUseAddResMenu = 1 shl 2;
	popupUseWFont = 1 shl 3;

{ Menu label styles for the System 7 pop-up menu}
const
	popupTitleBold = 1 shl 8;
	popupTitleItalic = 1 shl 9;
	popupTitleUnderline = 1 shl 10;
	popupTitleOutline = 1 shl 11;
	popupTitleShadow = 1 shl 12;
	popupTitleCondense = 1 shl 13;
	popupTitleExtend = 1 shl 14;
	popupTitleNoStyle = 1 shl 15;

{ Menu label justifications for the System 7 pop-up menu}
const
	popupTitleLeftJust = $00000000;
	popupTitleCenterJust = $00000001;
	popupTitleRightJust = $000000FF;

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

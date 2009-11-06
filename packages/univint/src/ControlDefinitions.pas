{
     File:       HIToolbox/ControlDefinitions.h
 
     Contains:   Definitions of controls provided by the Control Manager
 
     Version:    HIToolbox-437~1
 
     Copyright:  © 1999-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
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

unit ControlDefinitions;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,TextEdit,AXUIElement,AEDataModel,CFBase,Events,QuickdrawTypes,IconsCore,CFData,CFDictionary,CFString,DateTimeUtils,Drag,TextCommon,Appearance,CarbonEvents,Controls,Lists,MacHelp,Menus,HIObject;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


{
 *  ControlDefinitions.h
 *  
 *  Discussion:
 *    System software supplies a variety of controls for your
 *    applications to use. They are described herein.
 }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ Control Definition IDÕs                                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Standard System 7 procIDs}

const
	pushButProc = 0;
	checkBoxProc = 1;
	radioButProc = 2;
	scrollBarProc = 16;
	popupMenuProc = 1008;

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ Control Part Codes                                                                }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kControlLabelPart = 1;
	kControlMenuPart = 2;
	kControlTrianglePart = 4;
	kControlEditTextPart = 5;    { Appearance 1.0 and later}
	kControlPicturePart = 6;    { Appearance 1.0 and later}
	kControlIconPart = 7;    { Appearance 1.0 and later}
	kControlClockPart = 8;    { Appearance 1.0 and later}
	kControlListBoxPart = 24;   { Appearance 1.0 and later}
	kControlListBoxDoubleClickPart = 25;  { Appearance 1.0 and later}
	kControlImageWellPart = 26;   { Appearance 1.0 and later}
	kControlRadioGroupPart = 27;   { Appearance 1.0.2 and later}
	kControlButtonPart = 10;
	kControlCheckBoxPart = 11;
	kControlRadioButtonPart = 11;
	kControlUpButtonPart = kAppearancePartUpButton;
	kControlDownButtonPart = kAppearancePartDownButton;
	kControlPageUpPart = kAppearancePartPageUpArea;
	kControlPageDownPart = kAppearancePartPageDownArea;
	kControlClockHourDayPart = 9;    { Appearance 1.1 and later}
	kControlClockMinuteMonthPart = 10;   { Appearance 1.1 and later}
	kControlClockSecondYearPart = 11;   { Appearance 1.1 and later}
	kControlClockAMPMPart = 12;   { Appearance 1.1 and later}
	kControlDataBrowserPart = 24;   { CarbonLib 1.0 and later}
	kControlDataBrowserDraggedPart = 25;   { CarbonLib 1.0 and later}

{--------------------------------------------------------------------------------------}
{  ¥ DEPRECATED                                                                        }
{  All functions below this point are either deprecated (they continue to function     }
{  but are not the most modern nor most efficient solution to a problem), or they are  }
{  completely unavailable on Mac OS X.                                                 }
{--------------------------------------------------------------------------------------}
{
  ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    ¥ EDIT TEXT (CDEF 17)
  ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Use the EditUnicodeText control to replace the Edit Text control. It uses the same
    data tags as the Edit Text control.
}

{ Edit Text proc IDs }
const
	kControlEditTextProc = 272;
	kControlEditTextPasswordProc = 274;

{ proc IDs available with Appearance 1.1 or later }
const
	kControlEditTextInlineInputProc = 276; { Can't combine with the other variants}

{ Control Kind Tag }
const
	kControlKindEditText = FourCharCode('etxt');

{$ifc not TARGET_CPU_64}
{
 *  CreateEditTextControl()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use CreateEditUnicodeTextControl API instead.
 *  
 *  Summary:
 *    Creates a new edit text control.
 *  
 *  Discussion:
 *    This control is a legacy control. It is deprecated in favor of
 *    the EditUnicodeText control, which handles Unicode and draws its
 *    text using antialiasing.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    window:
 *      The window in which the control should be placed. May be NULL
 *      in 10.3 and later.
 *    
 *    boundsRect:
 *      The bounds of the control, in local coordinates of the window.
 *    
 *    text:
 *      The text of the control. May be NULL.
 *    
 *    isPassword:
 *      A Boolean indicating whether the field is to be used as a
 *      password field. Passing false indicates that the field is to
 *      display entered text normally. True means that the field will
 *      be used as a password field and any text typed into the field
 *      will be displayed only as bullets.
 *    
 *    useInlineInput:
 *      A Boolean indicating whether or not the control is to accept
 *      inline input. Pass true to to accept inline input, otherwise
 *      pass false.
 *    
 *    style:
 *      The control's font style, size, color, and so on. May be NULL.
 *    
 *    outControl:
 *      On exit, contains the new control (if noErr is returned as the
 *      result code).
 *  
 *  Result:
 *    An operating system result code.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreateEditTextControl( window: WindowRef; const (*var*) boundsRect: Rect; text: CFStringRef { can be NULL }; isPassword: Boolean; useInlineInput: Boolean; {const} style: ControlFontStyleRecPtr { can be NULL }; var outControl: ControlRef ): OSStatus; external name '_CreateEditTextControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 *)


{ ControlData tags supported only by the classic EditText control}
{$endc} {not TARGET_CPU_64}

const
	kControlEditTextTEHandleTag = FourCharCode('than'); { The TEHandle of the text edit record}
	kControlEditTextInlinePreUpdateProcTag = FourCharCode('prup'); { TSMTEPreUpdateUPP and TSMTEPostUpdateUpp. For use with inline input variant...}
	kControlEditTextInlinePostUpdateProcTag = FourCharCode('poup'); { ...The refCon parameter will contain the ControlRef.}

{
    The classic EditText control also supports these tags defined for the EditUnicodeText control:
    
        kControlEditTextLockedTag
        kControlEditTextStyleTag
        kControlEditTextFixedTextTag
        kControlEditTextTextTag
        kControlEditTextKeyFilterTag
        kControlEditTextValidationProcTag
        kControlEditTextSelectionTag
        kControlEditTextKeyScriptBehaviorTag
        kControlEditTextCFStringTag
        kControlEditTextPasswordTag
        kControlEditTextPasswordCFStringTag
}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ PICTURE CONTROL (CDEF 19)                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  Value parameter should contain the ID of the picture you wish to display when       }
{  creating controls of this type. If you don't want the control tracked at all, use   }
{  the 'no track' variant.                                                             }
{ Picture control proc IDs }
const
	kControlPictureProc = 304;
	kControlPictureNoTrackProc = 305;   { immediately returns kControlPicturePart}

{ Control Kind Tag }
const
	kControlKindPicture = FourCharCode('pict');

{ The HIObject class ID for the HIPictureView class. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kHIPictureViewClassID CFSTRP('com.apple.HIPictureView')}
{$endc}
{$ifc not TARGET_CPU_64}
{
 *  CreatePictureControl()
 *  
 *  Summary:
 *    Creates a picture control.
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
 *    content:
 *      The descriptor for the picture you want the control to display.
 *    
 *    dontTrack:
 *      A Boolean value indicating whether the control should hilite
 *      when it is clicked on. False means hilite and track the mouse.
 *    
 *    outControl:
 *      On exit, contains the new control.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreatePictureControl( window: WindowRef { can be NULL }; const (*var*) boundsRect: Rect; const (*var*) content: ControlButtonContentInfo; dontTrack: Boolean; var outControl: ControlRef ): OSStatus; external name '_CreatePictureControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Tagged data supported by picture controls }
{$endc} {not TARGET_CPU_64}

const
	kControlPictureHandleTag = FourCharCode('pich'); { PicHandle}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ LIST BOX (CDEF 22)                                                                }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  Lists use an auxiliary resource to define their format. The resource type used is   }
{  'ldes' and a definition for it can be found in Appearance.r. The resource ID for    }
{  the ldes is passed in the 'value' parameter when creating the control. You may pass }
{  zero in value. This tells the List Box control to not use a resource. The list will }
{  be created with default values, and will use the standard LDEF (0). You can change  }
{  the list by getting the list handle. You can set the LDEF to use by using the tag   }
{  below (kControlListBoxLDEFTag)                                                      }
{ List Box proc IDs }
const
	kControlListBoxProc = 352;
	kControlListBoxAutoSizeProc = 353;

{ Control Kind Tag }
const
	kControlKindListBox = FourCharCode('lbox');

{$ifc not TARGET_CPU_64}
{
 *  CreateListBoxControl()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreateListBoxControl( window: WindowRef; const (*var*) boundsRect: Rect; autoSize: Boolean; numRows: SInt16; numColumns: SInt16; horizScroll: Boolean; vertScroll: Boolean; cellHeight: SInt16; cellWidth: SInt16; hasGrowSpace: Boolean; const (*var*) listDef: ListDefSpec; var outControl: ControlRef ): OSStatus; external name '_CreateListBoxControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Tagged data supported by list box }
{$endc} {not TARGET_CPU_64}

const
	kControlListBoxListHandleTag = FourCharCode('lhan'); { ListHandle}
	kControlListBoxKeyFilterTag = kControlKeyFilterTag; { ControlKeyFilterUPP}
	kControlListBoxFontStyleTag = kControlFontStyleTag; { ControlFontStyleRec}

{ New tags in 1.0.1 or later }
const
	kControlListBoxDoubleClickTag = FourCharCode('dblc'); { Boolean. Was last click a double-click?}
	kControlListBoxLDEFTag = FourCharCode('ldef'); { SInt16. ID of LDEF to use.}

{ Resource Types }
const
	kControlListDescResType = FourCharCode('ldes'); { used for list box control (Appearance 1.0 and later)}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ SCROLL TEXT BOX (CDEF 27)                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  This control implements a scrolling box of (non-editable) text. This is useful for  }
{  credits in about boxes, etc.                                                        }
{  The standard version of this control has a scroll bar, but the autoscrolling        }
{  variant does not. The autoscrolling variant needs two pieces of information to      }
{  work: delay (in ticks) before the scrolling starts, and time (in ticks) between     }
{  scrolls. It will scroll one pixel at a time, unless changed via SetControlData.     }
{  Parameter                   What Goes Here                                          }
{  ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ         ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ    }
{  Value                       Resource ID of 'TEXT'/'styl' content.                   }
{  Min                         Scroll start delay (in ticks)                       .   }
{  Max                         Delay (in ticks) between scrolls.                       }
{  NOTE: This control is only available with Appearance 1.1.                           }
{ Scroll Text Box Proc IDs }
const
	kControlScrollTextBoxProc = 432;
	kControlScrollTextBoxAutoScrollProc = 433;

{ Control Kind Tag }
const
	kControlKindScrollingTextBox = FourCharCode('stbx');

{$ifc not TARGET_CPU_64}
{
 *  CreateScrollingTextBoxControl()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework [32-bit only]
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function CreateScrollingTextBoxControl( window: WindowRef; const (*var*) boundsRect: Rect; contentResID: SInt16; autoScroll: Boolean; delayBeforeAutoScroll: UInt32; delayBetweenAutoScroll: UInt32; autoScrollAmount: UInt16; var outControl: ControlRef ): OSStatus; external name '_CreateScrollingTextBoxControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Tagged data supported by Scroll Text Box }
{$endc} {not TARGET_CPU_64}

const
	kControlScrollTextBoxDelayBeforeAutoScrollTag = FourCharCode('stdl'); { UInt32 (ticks until autoscrolling starts)}
	kControlScrollTextBoxDelayBetweenAutoScrollTag = FourCharCode('scdl'); { UInt32 (ticks between scrolls)}
	kControlScrollTextBoxAutoScrollAmountTag = FourCharCode('samt'); { UInt16 (pixels per scroll) -- defaults to 1}
	kControlScrollTextBoxContentsTag = FourCharCode('tres'); { SInt16 (resource ID of 'TEXT'/'styl') -- write only!}
	kControlScrollTextBoxAnimatingTag = FourCharCode('anim'); { Boolean (whether the text box should auto-scroll)}

{$ifc OLDROUTINENAMES}
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  ¥ OLDROUTINENAMES                                                                   }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	inLabel = kControlLabelPart;
	inMenu = kControlMenuPart;
	inTriangle = kControlTrianglePart;
	inButton = kControlButtonPart;
	inCheckBox = kControlCheckBoxPart;
	inUpButton = kControlUpButtonPart;
	inDownButton = kControlDownButtonPart;
	inPageUp = kControlPageUpPart;
	inPageDown = kControlPageDownPart;

const
	kInLabelControlPart = kControlLabelPart;
	kInMenuControlPart = kControlMenuPart;
	kInTriangleControlPart = kControlTrianglePart;
	kInButtonControlPart = kControlButtonPart;
	kInCheckBoxControlPart = kControlCheckBoxPart;
	kInUpButtonControlPart = kControlUpButtonPart;
	kInDownButtonControlPart = kControlDownButtonPart;
	kInPageUpControlPart = kControlPageUpPart;
	kInPageDownControlPart = kControlPageDownPart;


{$endc}  {OLDROUTINENAMES}


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

{
     File:       HIToolbox/Appearance.h
 
     Contains:   Appearance Manager Interfaces.
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 1994-2005 by Apple Computer, Inc., all rights reserved
 
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

unit Appearance;
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
uses MacTypes,CFBase,CGContext,Collections,Processes,QuickdrawText,TextCommon,Quickdraw,TextEdit,QDOffscreen,MacErrors,TextUtils,CFString;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Appearance Manager constants, etc.                                               }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Appearance Manager Apple Events (1.1 and later)              }


{$ALIGN MAC68K}

const
	kAppearanceEventClass = $61707072 (* 'appr' *); { Event Class }
	kAEAppearanceChanged = $74686D65 (* 'thme' *); { Appearance changed (e.g. platinum to hi-tech) }
	kAESystemFontChanged = $73797366 (* 'sysf' *); { system font changed }
	kAESmallSystemFontChanged = $7373666E (* 'ssfn' *); { small system font changed }
	kAEViewsFontChanged = $76666E74 (* 'vfnt' *); { views font changed }

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Appearance Manager file types                                                    }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeDataFileType = $74686D65 (* 'thme' *); { file type for theme files }
	kThemePlatinumFileType = $706C746E (* 'pltn' *); { file type for platinum appearance }
	kThemeCustomThemesFileType = $7363656E (* 'scen' *); { file type for user themes }
	kThemeSoundTrackFileType = $74736E64 (* 'tsnd' *);

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Appearance Manager Supported Themes                                              }
{ Use CopyThemeIdentifier to get the current theme ID                              }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kThemeAppearancePlatinum CFSTRP('com.apple.theme.appearance.platinum')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kThemeAppearanceAqua CFSTRP('com.apple.theme.appearance.aqua')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kThemeAppearanceAquaBlue CFSTRP('com.apple.theme.appearance.aqua.blue')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kThemeAppearanceAquaGraphite CFSTRP('com.apple.theme.appearance.aqua.graphite')}
{$endc}


{
 *  AppearancePartCode
 *  
 *  Summary:
 *    These are part codes returned by a few of the hit testing
 *    Appearance APIs. Many of the Control Manager's ControlPartCodes
 *    are based on these part codes.
 }
type
	AppearancePartCode = SInt16;
const
{
   * This represents the lack of a part. It will be returned when the
   * Appearance Manager's hit testing logic determines that the input
   * point is not in any part of the widget.
   }
	kAppearancePartMetaNone = 0;

  {
   * This represents a widget which is not currently clickable because
   * it is disabled.
   }
	kAppearancePartMetaDisabled = 254;

  {
   * This represents a widget which is inactive, presumably because it
   * is in a window that is inactive.
   }
	kAppearancePartMetaInactive = 255;

  {
   * The part of a widget which indicates the widget's value. Scroll
   * bar thumbs and slider thumbs are the two main examples.
   }
	kAppearancePartIndicator = 129;

  {
   * The part of a widget which moves its value visually upward. Scroll
   * bar up arrows are the main example.
   }
	kAppearancePartUpButton = 20;

  {
   * The part of a widget which moves its value visually downward.
   * Scroll bar down arrows are the main example.
   }
	kAppearancePartDownButton = 21;

  {
   * The part of a widget which moves its value visually leftward.
   * Scroll bar left arrows are the main example.
   }
	kAppearancePartLeftButton = kAppearancePartUpButton;

  {
   * The part of a widget which moves its value visually rightward.
   * Scroll bar right arrows are the main example.
   }
	kAppearancePartRightButton = kAppearancePartDownButton;

  {
   * The part of a widget which moves its value visually upward one
   * whole page. Scroll bar page up areas are the main example.
   }
	kAppearancePartPageUpArea = 22;

  {
   * The part of a widget which moves its value visually downward one
   * whole page. Scroll bar page down areas are the main example.
   }
	kAppearancePartPageDownArea = 23;

  {
   * The part of a widget which moves its value visually leftward one
   * whole page. Scroll bar page left areas are the main example.
   }
	kAppearancePartPageLeftArea = kAppearancePartPageUpArea;

  {
   * The part of a widget which moves its value visually rightward one
   * whole page. Scroll bar page right areas are the main example.
   }
	kAppearancePartPageRightArea = kAppearancePartPageDownArea;


{
 *  AppearanceRegionCode
 *  
 *  Summary:
 *    These are region codes used by a few of window-related Appearance
 *    APIs. Many of the Window Manager's WindowRegionCodes are based on
 *    these region codes.
 }
type
	AppearanceRegionCode = UInt16;
const
	kAppearanceRegionTitleBar = 0;
	kAppearanceRegionTitleText = 1;
	kAppearanceRegionCloseBox = 2;
	kAppearanceRegionZoomBox = 3;
	kAppearanceRegionDrag = 5;
	kAppearanceRegionGrow = 6;
	kAppearanceRegionCollapseBox = 7;
	kAppearanceRegionTitleProxyIcon = 8;  { Mac OS 8.5 forward}
	kAppearanceRegionStructure = 32;
	kAppearanceRegionContent = 33;    { Content area of the window; empty when the window is collapsed}


{
 *  Discussion:
 *    ThemeBrushes
 }
const
{
   * Use with kModalWindowClass.
   }
	kThemeBrushDialogBackgroundActive = 1;

  {
   * Use with kModalWindowClass.
   }
	kThemeBrushDialogBackgroundInactive = 2;

  {
   * Use with kAlertWindowClass and kMovableAlertWindowClass.
   }
	kThemeBrushAlertBackgroundActive = 3;

  {
   * Use with kAlertWindowClass and kMovableAlertWindowClass.
   }
	kThemeBrushAlertBackgroundInactive = 4;

  {
   * Use with kDocumentWindowClass.
   }
	kThemeBrushModelessDialogBackgroundActive = 5;

  {
   * Use with kDocumentWindowClass.
   }
	kThemeBrushModelessDialogBackgroundInactive = 6;

  {
   * Use with kFloatingWindowClass and kUtilityWindowClass.
   }
	kThemeBrushUtilityWindowBackgroundActive = 7;

  {
   * Use with kFloatingWindowClass and kUtilityWindowClass.
   }
	kThemeBrushUtilityWindowBackgroundInactive = 8;

  {
   * The background used for Finder list views.
   }
	kThemeBrushListViewSortColumnBackground = 9;
	kThemeBrushListViewBackground = 10;
	kThemeBrushIconLabelBackground = 11;
	kThemeBrushListViewSeparator = 12;
	kThemeBrushChasingArrows = 13;
	kThemeBrushDragHilite = 14;

  {
   * Use with kDocumentWindowClass.
   }
	kThemeBrushDocumentWindowBackground = 15;
	kThemeBrushFinderWindowBackground = 16;


{
 *  Discussion:
 *    ThemeBrushes introduced in Appearance 1.1 (Mac OS 8.5) and later
 }
const
	kThemeBrushScrollBarDelimiterActive = 17;
	kThemeBrushScrollBarDelimiterInactive = 18;
	kThemeBrushFocusHighlight = 19;
	kThemeBrushPopupArrowActive = 20;
	kThemeBrushPopupArrowPressed = 21;
	kThemeBrushPopupArrowInactive = 22;
	kThemeBrushAppleGuideCoachmark = 23;
	kThemeBrushIconLabelBackgroundSelected = 24;
	kThemeBrushStaticAreaFill = 25;
	kThemeBrushActiveAreaFill = 26;
	kThemeBrushButtonFrameActive = 27;
	kThemeBrushButtonFrameInactive = 28;
	kThemeBrushButtonFaceActive = 29;
	kThemeBrushButtonFaceInactive = 30;
	kThemeBrushButtonFacePressed = 31;
	kThemeBrushButtonActiveDarkShadow = 32;
	kThemeBrushButtonActiveDarkHighlight = 33;
	kThemeBrushButtonActiveLightShadow = 34;
	kThemeBrushButtonActiveLightHighlight = 35;
	kThemeBrushButtonInactiveDarkShadow = 36;
	kThemeBrushButtonInactiveDarkHighlight = 37;
	kThemeBrushButtonInactiveLightShadow = 38;
	kThemeBrushButtonInactiveLightHighlight = 39;
	kThemeBrushButtonPressedDarkShadow = 40;
	kThemeBrushButtonPressedDarkHighlight = 41;
	kThemeBrushButtonPressedLightShadow = 42;
	kThemeBrushButtonPressedLightHighlight = 43;
	kThemeBrushBevelActiveLight = 44;
	kThemeBrushBevelActiveDark = 45;
	kThemeBrushBevelInactiveLight = 46;
	kThemeBrushBevelInactiveDark = 47;


{
 *  Discussion:
 *    ThemeBrushes introduced in Appearance 1.1.1 (Mac OS 9.0) and
 *    later.
 }
const
	kThemeBrushNotificationWindowBackground = 48;


{
 *  Discussion:
 *    ThemeBrushes introduced in Carbon. Available in Mac OS X, and
 *    Carbon Lib 1.3 and later.
 }
const
{
   * Use with kMovableModalWindowClass windows. Available in Mac OS X,
   * and CarbonLib 1.3 and later.
   }
	kThemeBrushMovableModalBackground = 49;

  {
   * Use with kSheetWindowClass and kSheetAlertWindowClass. This is the
   * backwardly compatible sheet background.
   * kThemeBrushSheetBackgroundTransparent is preferred. Available in
   * Mac OS X, and CarbonLib 1.3 and later.
   }
	kThemeBrushSheetBackgroundOpaque = 50;

  {
   * Use with kDrawerWindowClass. Available in Mac OS X, and CarbonLib
   * 1.3 and later.
   }
	kThemeBrushDrawerBackground = 51;


{
 *  Discussion:
 *    ThemeBrushes introduced in Carbon. Available in Mac OS X, and
 *    Carbon Lib 1.6 and later.
 }
const
{
   * Use with kToolbarWindowClass.
   }
	kThemeBrushToolbarBackground = 52;


{
 *  Discussion:
 *    ThemeBrushes introduced in Mac OS X 10.1. Available in Mac OS X
 *    10.1 and CarbonLib 1.6, and later.
 }
const
{
   * Use with kSheetWindowClass and kSheetAlertWindowClass. Not fully
   * transparent -- this brush is the semi-transparent background seen
   * with modern sheets. Available in Mac OS X 10.1 and CarbonLib 1.6,
   * and later.
   }
	kThemeBrushSheetBackgroundTransparent = 53;

  {
   * Available in Mac OS X 10.1 and CarbonLib 1.6, and later.
   }
	kThemeBrushMenuBackground = 54;

  {
   * Available in Mac OS X 10.1 and CarbonLib 1.6, and later.
   }
	kThemeBrushMenuBackgroundSelected = 55;


{
 *  Discussion:
 *    ThemeBrushes introduced in Mac OS X 10.3.
 }
const
{
   * For lists that use different colors as the background for odd and
   * even rows (like iTunes), this brush is the background for the odd
   * numbered rows. Available in Mac OS X 10.4 and later.
   }
	kThemeBrushListViewOddRowBackground = 56;

  {
   * For lists that use different colors as the background for odd and
   * even rows (like iTunes), this brush is the background for the even
   * numbered rows. Available in Mac OS X 10.4 and later.
   }
	kThemeBrushListViewEvenRowBackground = 57;

  {
   * The color for the divider lines drawn between columns in some list
   * views. This brush may have an alpha value associated with it.
   * Drawing this brush with QuickDraw or fetching it as an RGBColor
   * will not necessarily yield satisfactory results.
   }
	kThemeBrushListViewColumnDivider = 58;


{
 *  Discussion:
 *    ThemeBrush compatibility synonyms. The newer names are preferred.
 }
const
	kThemeBrushSheetBackground = kThemeBrushSheetBackgroundOpaque;


{
 *  Discussion:
 *    Theme meta-brushes. They are specific colors that do not change
 *    from theme to theme. Use them instead of using direct RGB values.
 }
const
	kThemeBrushBlack = -1;
	kThemeBrushWhite = -2;

  {
   * Available in Mac OS 10.1 and CarbonLib 1.6, and later.
   }
	kThemeBrushPrimaryHighlightColor = -3;

  {
   * Available in Mac OS 10.1 and CarbonLib 1.6, and later.
   }
	kThemeBrushSecondaryHighlightColor = -4;

  {
   * Available in Mac OS 10.1 and CarbonLib 1.6, and later.
   }
	kThemeBrushAlternatePrimaryHighlightColor = -5;

type
	ThemeBrush = SInt16;
const
	kThemeTextColorDialogActive = 1;
	kThemeTextColorDialogInactive = 2;
	kThemeTextColorAlertActive = 3;
	kThemeTextColorAlertInactive = 4;
	kThemeTextColorModelessDialogActive = 5;
	kThemeTextColorModelessDialogInactive = 6;
	kThemeTextColorWindowHeaderActive = 7;
	kThemeTextColorWindowHeaderInactive = 8;
	kThemeTextColorPlacardActive = 9;
	kThemeTextColorPlacardInactive = 10;
	kThemeTextColorPlacardPressed = 11;
	kThemeTextColorPushButtonActive = 12;
	kThemeTextColorPushButtonInactive = 13;
	kThemeTextColorPushButtonPressed = 14;
	kThemeTextColorBevelButtonActive = 15;
	kThemeTextColorBevelButtonInactive = 16;
	kThemeTextColorBevelButtonPressed = 17;
	kThemeTextColorPopupButtonActive = 18;
	kThemeTextColorPopupButtonInactive = 19;
	kThemeTextColorPopupButtonPressed = 20;
	kThemeTextColorIconLabel = 21;
	kThemeTextColorListView = 22;

{ Text Colors available in Appearance 1.0.1 or later }
const
	kThemeTextColorDocumentWindowTitleActive = 23;
	kThemeTextColorDocumentWindowTitleInactive = 24;
	kThemeTextColorMovableModalWindowTitleActive = 25;
	kThemeTextColorMovableModalWindowTitleInactive = 26;
	kThemeTextColorUtilityWindowTitleActive = 27;
	kThemeTextColorUtilityWindowTitleInactive = 28;
	kThemeTextColorPopupWindowTitleActive = 29;
	kThemeTextColorPopupWindowTitleInactive = 30;
	kThemeTextColorRootMenuActive = 31;
	kThemeTextColorRootMenuSelected = 32;
	kThemeTextColorRootMenuDisabled = 33;
	kThemeTextColorMenuItemActive = 34;
	kThemeTextColorMenuItemSelected = 35;
	kThemeTextColorMenuItemDisabled = 36;
	kThemeTextColorPopupLabelActive = 37;
	kThemeTextColorPopupLabelInactive = 38;


{ Text colors available in Appearance 1.1 or later }
const
	kThemeTextColorTabFrontActive = 39;
	kThemeTextColorTabNonFrontActive = 40;
	kThemeTextColorTabNonFrontPressed = 41;
	kThemeTextColorTabFrontInactive = 42;
	kThemeTextColorTabNonFrontInactive = 43;
	kThemeTextColorIconLabelSelected = 44;
	kThemeTextColorBevelButtonStickyActive = 45;
	kThemeTextColorBevelButtonStickyInactive = 46;

{ Text colors available in Appearance 1.1.1 or later }
const
	kThemeTextColorNotification = 47;


{ Text colors only available later than OS X 10.1.3 }
const
	kThemeTextColorSystemDetail = 48;

{ These values are specific colors that do not change from             }
{ theme to theme. You can use them instead of using direct RGB values. }
const
	kThemeTextColorBlack = -1;
	kThemeTextColorWhite = -2;

type
	ThemeTextColor = SInt16;
{ States to draw primitives: disabled, active, and pressed (hilited) }
const
	kThemeStateInactive = 0;
	kThemeStateActive = 1;
	kThemeStatePressed = 2;
	kThemeStateRollover = 6;
	kThemeStateUnavailable = 7;
	kThemeStateUnavailableInactive = 8;

{ obsolete name }
const
	kThemeStateDisabled = 0;

const
	kThemeStatePressedUp = 2;    { draw with up pressed     (increment/decrement buttons) }
	kThemeStatePressedDown = 3;     { draw with down pressed (increment/decrement buttons) }

type
	ThemeDrawState = UInt32;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Theme cursor selectors available in Appearance 1.1 or later                      }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

const
	kThemeArrowCursor = 0;
	kThemeCopyArrowCursor = 1;
	kThemeAliasArrowCursor = 2;
	kThemeContextualMenuArrowCursor = 3;
	kThemeIBeamCursor = 4;
	kThemeCrossCursor = 5;
	kThemePlusCursor = 6;
	kThemeWatchCursor = 7;    { Can Animate }
	kThemeClosedHandCursor = 8;
	kThemeOpenHandCursor = 9;
	kThemePointingHandCursor = 10;
	kThemeCountingUpHandCursor = 11;   { Can Animate }
	kThemeCountingDownHandCursor = 12;   { Can Animate }
	kThemeCountingUpAndDownHandCursor = 13; { Can Animate }
	kThemeSpinningCursor = 14;   { Can Animate }
	kThemeResizeLeftCursor = 15;
	kThemeResizeRightCursor = 16;
	kThemeResizeLeftRightCursor = 17;
	kThemeNotAllowedCursor = 18;   { available on Mac OS X 10.2 and later }

  {
   * Available in Mac OS X 10.3 or later.
   }
	kThemeResizeUpCursor = 19;

  {
   * Available in Mac OS X 10.3 or later.
   }
	kThemeResizeDownCursor = 20;

  {
   * Available in Mac OS X 10.3 or later.
   }
	kThemeResizeUpDownCursor = 21;

  {
   * A special cursor to indicate that letting up the mouse will cause
   * a dragged item to go away. When the item goes away, a poof cloud
   * animation should occur. This cursor should be updated dynamically
   * dependeding on whether the mouse up action will remove the item.
   * Available in Mac OS X 10.3 or later.
   }
	kThemePoofCursor = 22;

type
	ThemeCursor = UInt32;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Theme menu bar drawing states                                                    }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeMenuBarNormal = 0;
	kThemeMenuBarSelected = 1;

type
	ThemeMenuBarState = UInt16;
{ attributes }
const
	kThemeMenuSquareMenuBar = 1 shl 0;

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Theme menu drawing states                                                        }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeMenuActive = 0;
	kThemeMenuSelected = 1;
	kThemeMenuDisabled = 3;

type
	ThemeMenuState = UInt16;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ MenuType: add kThemeMenuTypeInactive to menu type for DrawThemeMenuBackground if entire  }
{ menu is inactive                                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeMenuTypePullDown = 0;
	kThemeMenuTypePopUp = 1;
	kThemeMenuTypeHierarchical = 2;
	kThemeMenuTypeInactive = $0100;

type
	ThemeMenuType = UInt16;
const
	kThemeMenuItemPlain = 0;
	kThemeMenuItemHierarchical = 1;    { item has hierarchical arrow}
	kThemeMenuItemScrollUpArrow = 2;    { for scrollable menus, indicates item is scroller}
	kThemeMenuItemScrollDownArrow = 3;
	kThemeMenuItemAtTop = $0100; { indicates item is being drawn at top of menu}
	kThemeMenuItemAtBottom = $0200; { indicates item is being drawn at bottom of menu}
	kThemeMenuItemHierBackground = $0400; { item is within a hierarchical menu}
	kThemeMenuItemPopUpBackground = $0800; { item is within a popped up menu}
	kThemeMenuItemHasIcon = $8000; { add into non-arrow type when icon present}
	kThemeMenuItemNoBackground = $4000; { don't draw the menu background while drawing this item (Mac OS X only)}

type
	ThemeMenuItemType = UInt16;

{
 *  Discussion:
 *    ThemeBackgroundKinds
 }
const
{
   * The theme background used to draw the fill of a tab pane.
   }
	kThemeBackgroundTabPane = 1;

  {
   * The theme background used to draw the fill of a window placard.
   }
	kThemeBackgroundPlacard = 2;

  {
   * The theme background used to draw the window header of a window of
   * a window that does not contain content that has a hard-lined top
   * edge. An example of this would be icon view in the non side panel
   * Finder.
   }
	kThemeBackgroundWindowHeader = 3;

  {
   * The theme background used to draw the window header of a window of
   * a window that does contain content that has a hard-lined top edge.
   * An example of this would be list view in the non side panel Finder.
   }
	kThemeBackgroundListViewWindowHeader = 4;

  {
   * The theme background used to draw the fill of a secondary group
   * box.
   }
	kThemeBackgroundSecondaryGroupBox = 5;

  {
   * A special theme brush for drawing metal backgrounds. Currently,
   * this brush only works with HIThemeDrawBackground.
   }
	kThemeBackgroundMetal = 6;

type
	ThemeBackgroundKind = UInt32;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Theme Collection tags for Get/SetTheme                                                   }
{  X ALERT: Please note that Get/SetTheme are severely neutered under Mac OS X at present. }
{           The first group of tags below are available to get under both 9 and X. The     }
{           second group is 9 only. None of the tags can be used in SetTheme on X, as it   }
{           is completely inert on X, and will return unimpErr.                            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeNameTag = $6E616D65 (* 'name' *); { Str255}
	kThemeVariantNameTag = $7661726E (* 'varn' *); { Str255}
	kThemeVariantBaseTintTag = $74696E74 (* 'tint' *); { RGBColor (10.1 and later)}
	kThemeHighlightColorTag = $68636F6C (* 'hcol' *); { RGBColor}
	kThemeScrollBarArrowStyleTag = $73626172 (* 'sbar' *); { ThemeScrollBarArrowStyle}
	kThemeScrollBarThumbStyleTag = $73627468 (* 'sbth' *); { ThemeScrollBarThumbStyle}
	kThemeSoundsEnabledTag = $736E6473 (* 'snds' *); { Boolean}
	kThemeDblClickCollapseTag = $636F6C6C (* 'coll' *); { Boolean}

const
	kThemeAppearanceFileNameTag = $74686D65 (* 'thme' *); { Str255}
	kThemeSystemFontTag = $6C677366 (* 'lgsf' *); { Str255}
	kThemeSmallSystemFontTag = $736D7366 (* 'smsf' *); { Str255}
	kThemeViewsFontTag = $76666E74 (* 'vfnt' *); { Str255}
	kThemeViewsFontSizeTag = $7666737A (* 'vfsz' *); { SInt16}
	kThemeDesktopPatternNameTag = $7061746E (* 'patn' *); { Str255}
	kThemeDesktopPatternTag = $70617474 (* 'patt' *); { <variable-length data> (flattened pattern)}
	kThemeDesktopPictureNameTag = $64706E6D (* 'dpnm' *); { Str255}
	kThemeDesktopPictureAliasTag = $6470616C (* 'dpal' *); { <alias handle>}
	kThemeDesktopPictureAlignmentTag = $6470616E (* 'dpan' *); { UInt32 (see the Picture Alignments below)}
	kThemeHighlightColorNameTag = $68636E6D (* 'hcnm' *); { Str255}
	kThemeExamplePictureIDTag = $65706963 (* 'epic' *); { SInt16}
	kThemeSoundTrackNameTag = $736E6474 (* 'sndt' *); { Str255}
	kThemeSoundMaskTag = $736D736B (* 'smsk' *); { UInt32}
	kThemeUserDefinedTag = $75736572 (* 'user' *); { Boolean (this should _always_ be true if present - used by Control Panel).}
	kThemeSmoothFontEnabledTag = $736D6F6F (* 'smoo' *); { Boolean}
	kThemeSmoothFontMinSizeTag = $736D6F73 (* 'smos' *); { UInt16 (must be >= 12 and <= 24)}

{ Picture Aligmnents that might be reported in the data for kThemeDesktopPictureAlignmentTag}
const
	kTiledOnScreen = 1;    { draws picture repeatedly}
	kCenterOnScreen = 2;    { "actual size", shows pattern on sides or clips picture if necessary}
	kFitToScreen = 3;    { shrinks if necessary}
	kFillScreen = 4;    { messes up aspect ratio if necessary}
	kUseBestGuess = 5;     { heuristically determines the best way to display the picture based on picture and monitor sizes}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Theme Control Settings                                                                   }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeCheckBoxClassicX = 0;    { check box with an 'X'}
	kThemeCheckBoxCheckMark = 1;     { check box with a real check mark}

type
	ThemeCheckBoxStyle = UInt16;
const
	kThemeScrollBarArrowsSingle = 0;    { single arrow on each end}
	kThemeScrollBarArrowsLowerRight = 1;   { double arrows only on right or bottom}

type
	ThemeScrollBarArrowStyle = UInt16;
const
	kThemeScrollBarThumbNormal = 0;    { normal, classic thumb size}
	kThemeScrollBarThumbProportional = 1;  { proportional thumbs}

type
	ThemeScrollBarThumbStyle = UInt16;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Font constants                                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{
 *  Summary:
 *    A ThemeFontID value is a virtual font ID which can be passed to
 *    one of the Appearance Manager's text-related routines. Within
 *    those routines, the ThemeFontID is mapped into the appropriate
 *    real font (or fonts), size, and style based on the system
 *    appearance (Platinum on Mac OS 9, Aqua on Mac OS X), the string
 *    to be rendered (if any), the language/ script that the app is
 *    running in, and possibly other factors. The ThemeFontIDs allow
 *    you to get the correct text appearance for the platform your app
 *    is currently running on.
 }
const
{
   * The font used to draw most interface elements. If you can't find a
   * more appropriate font from the list below, you should use this
   * one. This font is suitable for drawing titles on most custom
   * widgets/buttons, as well as most static text in dialogs and
   * windows.
   }
	kThemeSystemFont = 0;

  {
   * The font used to draw interface elements when space is at a
   * premium. It draws a slightly smaller font compared to
   * kThemeSystemFont.
   }
	kThemeSmallSystemFont = 1;

  {
   * Identical to kThemeSmallSystemFont, except it draws bolded (or
   * otherwise emphasized in some fashion appropriate to your
   * application's language/script).
   }
	kThemeSmallEmphasizedSystemFont = 2;

  {
   * The font used to draw file and folder names in Finder windows or
   * other browsable lists.
   }
	kThemeViewsFont = 3;    { The following ID's are only available with MacOS X or CarbonLib 1.3 and later}

  {
   * Identical to kThemeSystemFont, except it draws bolded (or
   * otherwise emphasized in some fashion appropriate to your
   * application's language/script). Only available on Mac OS X or
   * CarbonLib 1.3 or later.
   }
	kThemeEmphasizedSystemFont = 4;

  {
   * An analog to the Script Manager's notion of the Application Font.
   * This font is a suitable default choice for your application's
   * document-style text editing areas. Only available on Mac OS X or
   * CarbonLib 1.3 or later.
   }
	kThemeApplicationFont = 5;

  {
   * Generally smaller than kThemeSmallSystemFont, this font is
   * appropriate for drawing text labels next to image content that
   * reinforces the text's meaning (such as on a bevel button). Only
   * available on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeLabelFont = 6;

  {
   * The font used to draw menu titles in the menu bar. Only available
   * on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeMenuTitleFont = 100;

  {
   * The font used to draw menu items in the menus. Only available on
   * Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeMenuItemFont = 101;

  {
   * The font used to draw menu item marks in the menus. Only available
   * on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeMenuItemMarkFont = 102;

  {
   * The font used to draw menu item command key equivalents in the
   * menus. Only available on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeMenuItemCmdKeyFont = 103;

  {
   * The font used to draw text in most window title bars. Only
   * available on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeWindowTitleFont = 104;

  {
   * The font used to draw text labels on push buttons. Only available
   * on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemePushButtonFont = 105;

  {
   * The font used to draw text in utility window title bars. Only
   * available on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeUtilityWindowTitleFont = 106;

  {
   * The font used to draw the first (and most important) message of an
   * alert window. Only available on Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeAlertHeaderFont = 107;
	kThemeSystemFontDetail = 7;
	kThemeSystemFontDetailEmphasized = 8;

  {
   * Unlike the other ThemeFontIDs, this one doesn't map to a font
   * appropriate to your application's language or script. It maps
   * directly to the font, size, and style of the current Quickdraw
   * port. This allows you to get somewhat customized behavior out of
   * the APIs which take ThemeFontIDs. Note, however, that
   * kThemeCurrentPortFont does not (and will never) support all
   * Quickdraw styles on all platforms; in particular, outline and
   * shadow style are not supported on Mac OS X. Additionally,
   * kThemeCurrentPortFont is not (and will never be) completely
   * unicode savvy; use of kThemeCurrentPortFont may result in errors
   * having to do with the current port's font not being appropriate
   * for rendering or measuring all glyphs in a given unicode string.
   * Because of overhead associated with gathering Quickdraw font
   * information and converting it to the native font format on Mac OS
   * X, use of kThemeCurrentPortFont may slow down your text drawing
   * and measuring significantly compared to other ThemeFontIDs.
   * Instead of using kThemeCurrentPortFont, your application will
   * probably be better served by using one of the other ThemeFontIDs;
   * use kThemeCurrentPortFont only as a last resort. Only available on
   * Mac OS X or CarbonLib 1.3 or later.
   }
	kThemeCurrentPortFont = 200;

  {
   * The font used to draw the label of a toolbar item. Available in
   * Mac OS X 10.2 or later.
   }
	kThemeToolbarFont = 108;

  {
   * The appropriate system font for mini-sized controls. Available in
   * Mac OS X 10.3 or later.
   }
	kThemeMiniSystemFont = 109;

{ This is the total of the PUBLIC ThemeFontIDs!}
const
	kPublicThemeFontCount = 20;

type
	ThemeFontID = UInt16;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Tab constants                                                                            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeTabNonFront = 0;
	kThemeTabNonFrontPressed = 1;
	kThemeTabNonFrontInactive = 2;
	kThemeTabFront = 3;
	kThemeTabFrontInactive = 4;
	kThemeTabNonFrontUnavailable = 5;
	kThemeTabFrontUnavailable = 6;

type
	ThemeTabStyle = UInt16;
const
	kThemeTabNorth = 0;
	kThemeTabSouth = 1;
	kThemeTabEast = 2;
	kThemeTabWest = 3;

type
	ThemeTabDirection = UInt16;

{
 *  Summary:
 *    Deprecated tab height and overlap constants.
 *  
 *  Discussion:
 *    These constants have been deprecated in favor of theme metrics.
 *    Please do not use them anymore. These constants will be removed
 *    in the next major release of OS X.
 }
const
{
   * Deprecated. Use kThemeMetricSmallTabHeight.
   }
	kThemeSmallTabHeight = 16;

  {
   * Deprecated. Use kThemeMetricLargeTabHeight.
   }
	kThemeLargeTabHeight = 21;

  {
   * Deprecated. Use kThemeMetricTabFrameOverlap.
   }
	kThemeTabPaneOverlap = 3;

  {
   * Deprecated. Use kThemeMetricSmallTabHeight and
   * kThemeMetricSmallTabFrameOverlap.
   }
	kThemeSmallTabHeightMax = 19;

  {
   * Deprecated. Use metric kThemeMetricLargeTabHeight and
   * kThemeMetricTabFrameOverlap.
   }
	kThemeLargeTabHeightMax = 24;


{
 *  Discussion:
 *    ThemeTrackKinds
 }
const
{
   * This is the primitive used to draw the normal variant of the
   * scroll bar control.
   }
	kThemeScrollBarMedium = 0;

  {
   * This is the primitive used to draw the small variant of the scroll
   * bar control.
   }
	kThemeScrollBarSmall = 1;

  {
   * This is the primitive used to draw the normal variant of the
   * slider control.
   }
	kThemeSliderMedium = 2;

  {
   * This is the primitive used to draw the normal variant of the
   * progress bar control.
   }
	kThemeProgressBarMedium = 3;

  {
   * This is the primitive used to draw the normal variant of the
   * indeterminate progress bar control.
   }
	kThemeIndeterminateBarMedium = 4;

  {
   * This is the primitive used to draw the normal variant of the
   * relevance bar control.
   }
	kThemeRelevanceBar = 5;

  {
   * This is the primitive used to draw the small variant of the slider
   * control.
   }
	kThemeSliderSmall = 6;

  {
   * This is the primitive used to draw the large variant of the
   * progress bar control.
   }
	kThemeProgressBarLarge = 7;

  {
   * This is the primitive used to draw the large variant of the
   * indeterminate progress bar control.
   }
	kThemeIndeterminateBarLarge = 8;


{
 *  Discussion:
 *    New ThemeTrackKinds on Mac OS X 10.3 and later. Not all of them
 *    are implemented.
 }
const
{
   * Not implemented. Will return paramErr if used.
   }
	kThemeScrollBarMini = 9;

  {
   * A miniature version of the slider.
   }
	kThemeSliderMini = 10;

  {
   * Not implemented. Will return paramErr if used.
   }
	kThemeProgressBarMini = 11;

  {
   * Not implemented. Will return paramErr if used.
   }
	kThemeIndeterminateBarMini = 12;


{
 *  Discussion:
 *    These are legacy synonyms for previously defined ThemeTrackKinds.
 *    Please use the modern constant names.
 }
const
	kThemeMediumScrollBar = kThemeScrollBarMedium;
	kThemeSmallScrollBar = kThemeScrollBarSmall;
	kThemeMediumSlider = kThemeSliderMedium;
	kThemeMediumProgressBar = kThemeProgressBarMedium;
	kThemeMediumIndeterminateBar = kThemeIndeterminateBarMedium;
	kThemeSmallSlider = kThemeSliderSmall;
	kThemeLargeProgressBar = kThemeProgressBarLarge;
	kThemeLargeIndeterminateBar = kThemeIndeterminateBarLarge;
	kThemeMiniScrollBar = kThemeScrollBarMini;
	kThemeMiniSlider = kThemeSliderMini;
	kThemeMiniProgressBar = kThemeProgressBarMini;
	kThemeMiniIndeterminateBar = kThemeIndeterminateBarMini;

type
	ThemeTrackKind = UInt16;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Track enable states                                                                      }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
{ track states }
	kThemeTrackActive = 0;
	kThemeTrackDisabled = 1;
	kThemeTrackNothingToScroll = 2;
	kThemeTrackInactive = 3;

type
	ThemeTrackEnableState = UInt8;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Track pressed states                                                                     }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
{ press states (ignored unless track is active) }
	kThemeLeftOutsideArrowPressed = $01;
	kThemeLeftInsideArrowPressed = $02;
	kThemeLeftTrackPressed = $04;
	kThemeThumbPressed = $08;
	kThemeRightTrackPressed = $10;
	kThemeRightInsideArrowPressed = $20;
	kThemeRightOutsideArrowPressed = $40;
	kThemeTopOutsideArrowPressed = kThemeLeftOutsideArrowPressed;
	kThemeTopInsideArrowPressed = kThemeLeftInsideArrowPressed;
	kThemeTopTrackPressed = kThemeLeftTrackPressed;
	kThemeBottomTrackPressed = kThemeRightTrackPressed;
	kThemeBottomInsideArrowPressed = kThemeRightInsideArrowPressed;
	kThemeBottomOutsideArrowPressed = kThemeRightOutsideArrowPressed;

type
	ThemeTrackPressState = UInt8;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Thumb directions                                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
{ thumb direction }
	kThemeThumbPlain = 0;
	kThemeThumbUpward = 1;
	kThemeThumbDownward = 2;

type
	ThemeThumbDirection = UInt8;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Track attributes                                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{
 *  Discussion:
 *    Theme track attributes control the look of the track elements as
 *    drawn by the DrawThemeTrackFoo as well as the region returned by
 *    GetThemeTrackFoo.
 }
const
{
   * The track is drawn horizontally.
   }
	kThemeTrackHorizontal = 1 shl 0;

  {
   * The track progresses from right to left.
   }
	kThemeTrackRightToLeft = 1 shl 1;

  {
   * The track's thumb should be drawn.
   }
	kThemeTrackShowThumb = 1 shl 2;

  {
   * The provided thumbRgn should be drawn opaque, not as a ghost.
   }
	kThemeTrackThumbRgnIsNotGhost = 1 shl 3;

  {
   * The track scroll bar doesn't have arrows.  This attribute
   * currently has no effect
   }
	kThemeTrackNoScrollBarArrows = 1 shl 4;

  {
   * The thumb has focus.  This attribute currently has effect only on
   * sliders.  Available only in Mac OS X after 10.1.
   }
	kThemeTrackHasFocus = 1 shl 5;

type
	ThemeTrackAttributes = UInt16;

{
 *  ScrollBarTrackInfo
 *  
 *  Summary:
 *    Drawing parameters passed to scroll bar drawing and measuring
 *    theme APIs.
 *  
 *  Discussion:
 *    Used in both the old theme APIs and the newer HITheme APIs.
 }
type
	ScrollBarTrackInfoPtr = ^ScrollBarTrackInfo;
	ScrollBarTrackInfo = record
{
   * The current view range size.
   }
		viewsize: SInt32;               { current view range size }

  {
   * ThemeTrackPressState bits that indicate the pressed parts state.
   }
		pressState: SInt8 {ThemeTrackPressState};           { pressed parts state }
		pad: SInt8
	end;

{
 *  SliderTrackInfo
 *  
 *  Summary:
 *    Drawing parameters passed to slider drawing and measuring theme
 *    APIs.
 *  
 *  Discussion:
 *    Used in both the old theme APIs and the newer HITheme APIs.
 }
type
	SliderTrackInfoPtr = ^SliderTrackInfo;
	SliderTrackInfo = record
{
   * A ThemeThumbDirection indicating the thumb direction.
   }
		thumbDir: SInt8 {ThemeThumbDirection};              { thumb direction }

  {
   * ThemeTrackPressState bits that indicate the pressed parts state.
   }
		pressState: SInt8 {ThemeTrackPressState};           { pressed parts state }
	end;

{
 *  ProgressTrackInfo
 *  
 *  Summary:
 *    Drawing parameters passed to (indeterminate) progress bar drawing
 *    and measuring theme APIs.
 *  
 *  Discussion:
 *    Used in both the old theme APIs and the newer HITheme APIs.
 }
type
	ProgressTrackInfoPtr = ^ProgressTrackInfo;
	ProgressTrackInfo = record
{
   * A UInt8 indicating the current phase of the track fill.
   }
		phase: SInt8;                  { phase for indeterminate progress }
		pad: SInt8
	end;

{
 *  ThemeTrackDrawInfo
 *  
 *  Summary:
 *    Drawing parameters passed to track drawing and measuring theme
 *    APIs.
 *  
 *  Discussion:
 *    Use the newer HITheme APIs.
 }
type
	ThemeTrackDrawInfoPtr = ^ThemeTrackDrawInfo;
	ThemeTrackDrawInfo = record
{
   * The ThemeTrackKind of the track being drawn or measured.
   }
		kind: ThemeTrackKind;

  {
   * The bounds of the track is being drawn or measured.
   }
		bounds: Rect;

  {
   * The minimum allowable value for the track being drawn or measured.
   }
		min: SInt32;

  {
   * The maximum allowable value for the track being drawn or measured.
   }
		max: SInt32;

  {
   * The value for the track being drawn or measured.
   }
		value: SInt32;

  {
   * Leave this reserved field set to 0.
   }
		reserved: UInt32;


  {
   * A set of ThemeTrackAttributes for the track to be drawn or
   * measured.
   }
		attributes: ThemeTrackAttributes;

  {
   * A ThemeTrackEnableState describing the state of the track to be
   * drawn or measured.
   }
		enableState: SInt8 {ThemeTrackEnableState};

  {
   * Leave this reserved field set to 0.
   }
		filler1: SInt8;
		case SInt16 of
		0: (
			scrollbar:			ScrollBarTrackInfo;
			);
		1: (
			slider:				SliderTrackInfo;
			);
		2: (
			progress:			ProgressTrackInfo;
			);
	end;

{
 *  Summary:
 *    ThemeWindowAttributes
 *  
 *  Discussion:
 *    Theme window attributes control the look of the window elements
 *    as drawn by the DrawThemeWindowFrame.
 }
const
{
   * This bit means that the size of the window can be changed by the
   * user. The window is drawn with a grow box.
   }
	kThemeWindowHasGrow = 1 shl 0;

  {
   * This bit means that the window can be zoomed horizontally. The
   * window is drawn with a zoom box.
   }
	kThemeWindowHasHorizontalZoom = 1 shl 3;

  {
   * This bit means that the window can be zoomed vertically. The
   * window is drawn with a zoom box.
   }
	kThemeWindowHasVerticalZoom = 1 shl 4;

  {
   * This bit means that the window can be zoomed horizontally and
   * vertically. The window is drawn with a zoom box.
   }
	kThemeWindowHasFullZoom = kThemeWindowHasHorizontalZoom + kThemeWindowHasVerticalZoom;

  {
   * This bit means that the window can be closed. The window is drawn
   * with a close box.
   }
	kThemeWindowHasCloseBox = 1 shl 5;

  {
   * This bit means that the window can be collapsed. The window is
   * drawn with a collapse box.
   }
	kThemeWindowHasCollapseBox = 1 shl 6;

  {
   * This bit means that the window has title text and/or a title icon.
   }
	kThemeWindowHasTitleText = 1 shl 7;

  {
   * This bit means that the window has been collapsed.
   }
	kThemeWindowIsCollapsed = 1 shl 8;

  {
   * This bit means that the window has dirty content. Get your mind
   * out of the gutter. That means that the content of the window has
   * changed and needs to be saved. If the window is drawing a close
   * box, it will be drawn dirty.
   }
	kThemeWindowHasDirty = 1 shl 9;

  {
   * This bit means that the window has a toolbar toggling button.
   }
	kThemeWindowHasToolbarButton = 1 shl 11;


type
	ThemeWindowAttributes = UInt32;

{
 *  Summary:
 *    ThemeWindowTypes
 *  
 *  Discussion:
 *    Window Types Supported by the Appearance Manager.
 }
const
	kThemeDocumentWindow = 0;
	kThemeDialogWindow = 1;
	kThemeMovableDialogWindow = 2;
	kThemeAlertWindow = 3;
	kThemeMovableAlertWindow = 4;
	kThemePlainDialogWindow = 5;
	kThemeShadowDialogWindow = 6;
	kThemePopupWindow = 7;
	kThemeUtilityWindow = 8;
	kThemeUtilitySideWindow = 9;
	kThemeSheetWindow = 10;
	kThemeDrawerWindow = 11;

type
	ThemeWindowType = UInt16;

{
 *  Summary:
 *    ThemeTitleBarWidgets
 *  
 *  Discussion:
 *    Window Widgets Supported by the Appearance Manager.
 }
const
	kThemeWidgetCloseBox = 0;
	kThemeWidgetZoomBox = 1;
	kThemeWidgetCollapseBox = 2;
	kThemeWidgetDirtyCloseBox = 6;

const
{ Old/Obsolete name to be removed}
	kThemeWidgetABox = 3;
	kThemeWidgetBBox = 4;
	kThemeWidgetBOffBox = 5;

type
	ThemeTitleBarWidget = UInt16;

{
 *  Summary:
 *    ThemeArrowOrientation
 *  
 *  Discussion:
 *    Popup arrow orientations.
 }
const
	kThemeArrowLeft = 0;
	kThemeArrowDown = 1;
	kThemeArrowRight = 2;
	kThemeArrowUp = 3;

type
	ThemeArrowOrientation = UInt16;

{
 *  Summary:
 *    ThemePopupArrowSizes
 *  
 *  Discussion:
 *    Popup arrow sizes.
 }
const
	kThemeArrow3pt = 0;
	kThemeArrow5pt = 1;
	kThemeArrow7pt = 2;
	kThemeArrow9pt = 3;

type
	ThemePopupArrowSize = UInt16;

{
 *  Summary:
 *    ThemeGrowDirections
 *  
 *  Discussion:
 *    Grow box directions.
 }
const
	kThemeGrowLeft = 1 shl 0; { can grow to the left }
	kThemeGrowRight = 1 shl 1; { can grow to the right }
	kThemeGrowUp = 1 shl 2; { can grow up }
	kThemeGrowDown = 1 shl 3; { can grow down }

type
	ThemeGrowDirection = UInt16;

{
 *  Discussion:
 *    ThemeButtonKinds
 }
const
{
   * Dynamically-sized push button kind. Prior to Mac OS X 10.3 all
   * push button sizes could be determined dynamically: either they
   * were smaller than normal size and therefore small or they were
   * normal size. This constant will never describe a mini push button,
   * regardless of associated bounds. Please use the explicitly-sized
   * kThemePushButton(Normal,Small,Mini) constants.
   }
	kThemePushButton = 0;
	kThemeCheckBox = 1;
	kThemeRadioButton = 2;

  {
   * Bevel button (obsolete)
   }
	kThemeBevelButton = 3;

  {
   * Popup button without text (no label). See ThemeButtonAdornment for
   * glyphs. The arrow button theme name is somewhat confusing. This is
   * the primitive used to draw the control known as the disclosure
   * button.
   }
	kThemeArrowButton = 4;

  {
   * Dynamically-sized popup button kind. Prior to Mac OS X 10.3 all
   * popup button sizes could be determined dynamically: either they
   * were smaller than normal size and therefore small or they were
   * normal size. This constant will never describe a mini popup
   * button, regardless of associated bounds. Please use the
   * explicitly-sized kThemePopupButton(Normal,Small,Mini) constants.
   }
	kThemePopupButton = 5;

  {
   * This is a disclosure triangle with a label beside it. Used to be
   * confusingly named kThemeDisclosureTriangle.
   }
	kThemeDisclosureTriangle = 6;

  {
   * Increment/decrement buttons (no label). This is the primitive used
   * to draw the LittleArrows control.
   }
	kThemeIncDecButton = 7;

  {
   * Small-shadow bevel button
   }
	kThemeBevelButtonSmall = 8;

  {
   * Med-shadow bevel button
   }
	kThemeBevelButtonMedium = 3;

  {
   * Large-shadow bevel button
   }
	kThemeBevelButtonLarge = 9;

  {
   * Sort button for top of a list. This is the theme primitive used to
   * draw the top of the columns in the data browser.
   }
	kThemeListHeaderButton = 10;

  {
   * This is the primitive used to draw the normal variant of the round
   * button control.
   }
	kThemeRoundButton = 11;

  {
   * This is the primitive used to draw the large variant of the round
   * button control.
   }
	kThemeRoundButtonLarge = 12;

  {
   * This is the primitive used to draw the small variant of the check
   * box control.
   }
	kThemeCheckBoxSmall = 13;

  {
   * This is the primitive used to draw the small variant of the radio
   * button control.
   }
	kThemeRadioButtonSmall = 14;

  {
   * This is the primitive used to draw the rounded-corner variant of
   * the bevel button control.
   }
	kThemeRoundedBevelButton = 15;

  {
   * This is the primitive used to draw the normal variant of the combo
   * box control.
   }
	kThemeComboBox = 16;

  {
   * This is the primitive used to draw the small variant of the combo
   * box control.
   }
	kThemeComboBoxSmall = 17;


{
 *  Discussion:
 *    New ThemeButtonKinds available on Mac OS X 10.3 and later.
 }
const
{
   * This is the primitive used to draw the mini variant of the combo
   * box control.
   }
	kThemeComboBoxMini = 18;

  {
   * This is the primitive used to draw the mini variant of the check
   * box control.
   }
	kThemeCheckBoxMini = 19;

  {
   * This is the primitive used to draw the mini variant of the radio
   * button control.
   }
	kThemeRadioButtonMini = 20;

  {
   * This is the primitive used to draw the small variant of the
   * LittleArrows control.
   }
	kThemeIncDecButtonSmall = 21;

  {
   * This is the primitive used to draw the mini variant of the
   * LittleArrows control.
   }
	kThemeIncDecButtonMini = 22;

  {
   * The arrow button theme name is somewhat confusing. This is the
   * primitive used to draw the small variant of the control known as
   * the disclosure button.
   }
	kThemeArrowButtonSmall = 23;

  {
   * The arrow button theme name is somewhat confusing. This is the
   * primitive used to draw the mini variant of the control known as
   * the disclosure button.
   }
	kThemeArrowButtonMini = 24;

  {
   * Explicitly-sized normal push button kind. Prior to Mac OS X 10.3
   * all push button sizes could be determined dynamically: either they
   * were smaller than normal size and therefore small or they were
   * normal size. Using this constant, an explicitly-sized normal push
   * button can be specified.
   }
	kThemePushButtonNormal = 25;

  {
   * Explicitly-sized small push button kind. Prior to Mac OS X 10.3
   * all push button sizes could be determined dynamically: either they
   * were smaller than normal size and therefore small or they were
   * normal size. Using this constant, an explicitly-sized small push
   * button can be specified.
   }
	kThemePushButtonSmall = 26;

  {
   * Explicitly-sized mini push button kind. Prior to Mac OS X 10.3 all
   * push button sizes could be determined dynamically: either they
   * were smaller than normal size and therefore small or they were
   * normal size. Since a mini variant was introduced in Mac OS X 10.3,
   * smaller than normal size is can also mean mini. To avoid confusion
   * with existing code, the mini variant will never be implicitly
   * determined and must be explicity requested with the
   * kThemePushButtonMini constant.
   }
	kThemePushButtonMini = 27;

  {
   * Explicitly-sized normal popup button kind. Prior to Mac OS X 10.3
   * all popup button sizes could be determined dynamically: either
   * they were smaller than normal size and therefore small or they
   * were normal size. Using this constant, an explicitly-sized normal
   * popup button can be specified.
   }
	kThemePopupButtonNormal = 28;

  {
   * Explicitly-sized small popup button kind. Prior to Mac OS X 10.3
   * all popup button sizes could be determined dynamically: either
   * they were smaller than normal size and therefore small or they
   * were normal size. Using this constant, an explicitly-sized small
   * popup button can be specified.
   }
	kThemePopupButtonSmall = 29;

  {
   * Explicitly-sized mini popup button kind. Prior to Mac OS X 10.3
   * all popup button sizes could be determined dynamically: either
   * they were smaller than normal size and therefore small or they
   * were normal size. Since a mini variant was introduced in Mac OS X
   * 10.3, smaller than normal size is can also mean mini. To avoid
   * confusion with existing code, the mini variant will never be
   * implicitly determined and must be explicity requested with the
   * kThemePopupButtonMini constant.
   }
	kThemePopupButtonMini = 30;


{
 *  Discussion:
 *    New ThemeButtonKinds available on Mac OS X 10.4 and later.
 }
const
{
   * This is the primitive used to draw the inset variant of the bevel
   * button control.
   }
	kThemeBevelButtonInset = 31;

  {
   * This is the primitive used to draw the inset variant of the push
   * button. Similar to kThemePushButtonNormal, but inset.
   }
	kThemePushButtonInset = 32;

  {
   * This is the primitive used to draw the small, inset variant of the
   * push button. Similar to kThemePushButtonSmall, but inset.
   }
	kThemePushButtonInsetSmall = 33;

  {
   * This is the primitive used to draw the help variant of the round
   * button.
   }
	kThemeRoundButtonHelp = 34;


{
 *  Discussion:
 *    These are legacy synonyms for previously defined
 *    ThemeButtonKinds. Please use the modern constant names.
 }
const
	kThemeNormalCheckBox = kThemeCheckBox;
	kThemeNormalRadioButton = kThemeRadioButton;
	kThemeLargeBevelButton = kThemeBevelButtonLarge;
	kThemeMediumBevelButton = kThemeBevelButtonMedium;
	kThemeMiniCheckBox = kThemeCheckBoxMini;
	kThemeMiniRadioButton = kThemeRadioButtonMini;
	kThemeSmallBevelButton = kThemeBevelButtonSmall;
	kThemeSmallCheckBox = kThemeCheckBoxSmall;
	kThemeSmallRadioButton = kThemeRadioButtonSmall;
	kThemeLargeRoundButton = kThemeRoundButtonLarge;
	kThemeDisclosureButton = kThemeDisclosureTriangle;

type
	ThemeButtonKind = UInt16;

{
 *  Summary:
 *    ThemeButtonValues
 *  
 *  Discussion:
 *    Common button values
 }
const
	kThemeButtonOff = 0;
	kThemeButtonOn = 1;
	kThemeButtonMixed = 2;
	kThemeDisclosureRight = 0;
	kThemeDisclosureDown = 1;
	kThemeDisclosureLeft = 2;

type
	ThemeButtonValue = UInt16;


{
 *  Summary:
 *    ThemeButtonAdornments
 *  
 *  Discussion:
 *    Use these adornments when drawing buttons using the theme
 *    primitive. Note that the bits starting at (1 << 6) are recycled
 *    and have different meanings depending on ThemeButtonKind.
 }
const
{
   * No adornments.
   }
	kThemeAdornmentNone = 0;

  {
   * Draw default ornamentation (for push button and generic well).
   }
	kThemeAdornmentDefault = 1 shl 0;

  {
   * Draw focus.
   }
	kThemeAdornmentFocus = 1 shl 2;

  {
   * Draw right to left label.
   }
	kThemeAdornmentRightToLeft = 1 shl 4;

  {
   * Don't draw or erase label (radio, check, disclosure).
   }
	kThemeAdornmentDrawIndicatorOnly = 1 shl 5;

  {
   * Draw the left border of the button as selected (list header button
   * only).
   }
	kThemeAdornmentHeaderButtonLeftNeighborSelected = 1 shl 6;

  {
   * Draw the right border of the button (list header button only).
   }
	kThemeAdornmentHeaderButtonRightNeighborSelected = 1 shl 7;

  {
   * Draw the sort indicator pointing upward (list header button only).
   }
	kThemeAdornmentHeaderButtonSortUp = 1 shl 8;

  {
   * Draw as a header menu button (list header button only).
   }
	kThemeAdornmentHeaderMenuButton = 1 shl 9;

  {
   * Draw the non-shadow area of the button (list header button only).
   }
	kThemeAdornmentHeaderButtonNoShadow = 1 shl 10;

  {
   * Draw the only the shadow area of the button (list header button
   * only).
   }
	kThemeAdornmentHeaderButtonShadowOnly = 1 shl 11;

  {
   * When drawn selected, do not draw the sort direction arrow (list
   * header button only). Mac OS X 10.4 and later.
   }
	kThemeAdornmentHeaderButtonNoSortArrow = 1 shl 12;

  {
   * Draw a left arrow on the arrow button or draw left disclosure
   * triangle. Can be combined with kThemeAdornmentArrowDownArrow to
   * draw an intermediate disclosure triangle.
   }
	kThemeAdornmentArrowLeftArrow = 1 shl 6;

  {
   * Draw a down arrow on the arrow button or draw right disclosure
   * triangle. Can be combined with kThemeAdornmentArrowLeftArrow to
   * draw an intermediate disclosure triangle.
   }
	kThemeAdornmentArrowDownArrow = 1 shl 7;

  {
   * Draw a double arrow on the arrow button.
   }
	kThemeAdornmentArrowDoubleArrow = 1 shl 8;

  {
   * Draw a up arrow on the arrow button.
   }
	kThemeAdornmentArrowUpArrow = 1 shl 9;


{
 *  Discussion:
 *    These are legacy synonyms for previously defined
 *    ThemeButtonAdornments. Please use the modern constant names.
 }
const
	kThemeAdornmentNoShadow = kThemeAdornmentHeaderButtonNoShadow;
	kThemeAdornmentShadowOnly = kThemeAdornmentHeaderButtonShadowOnly;

type
	ThemeButtonAdornment = UInt16;

{
 *  ThemeButtonDrawInfo
 *  
 *  Summary:
 *    Drawing parameters passed to button drawing and measuring theme
 *    APIs.
 *  
 *  Discussion:
 *    Use the newer HITheme APIs.
 }
type
	ThemeButtonDrawInfo = record
{
   * The ThemeDrawState of the button being drawn or measured.
   }
		state: ThemeDrawState;

  {
   * The ThemeButtonValue of the button being drawn or measured.
   }
		value: ThemeButtonValue;

  {
   * The ThemeButtonAdornment(s) with which the button is being drawn
   * or measured.
   }
		adornment: ThemeButtonAdornment;
	end;
	ThemeButtonDrawInfoPtr = ^ThemeButtonDrawInfo;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Sound Support                                                                            }
{  X ALERT: Please note that none of the theme sound APIs currently function on X.         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Sound classes                                                                            }
{ You can use the constants below to set what sounds are active using the SetTheme API.    }
{ Use these with the kThemeSoundMask tag.                                                  }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeNoSounds = 0;
	kThemeWindowSoundsMask = 1 shl 0;
	kThemeMenuSoundsMask = 1 shl 1;
	kThemeControlSoundsMask = 1 shl 2;
	kThemeFinderSoundsMask = 1 shl 3;


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Drag Sounds                                                                              }
{ Drag sounds are looped for the duration of the drag.                                     }
{ Call BeginThemeDragSound at the start of the drag.                                       }
{ Call EndThemeDragSound when the drag has finished.                                       }
{ Note that in order to maintain a consistent user experience, only one drag sound may     }
{ occur at a time.  The sound should be attached to a mouse action, start after the        }
{ mouse goes down and stop when the mouse is released.                                     }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeDragSoundNone = 0;
	kThemeDragSoundMoveWindow = $776D6F76 (* 'wmov' *);
	kThemeDragSoundGrowWindow = $7767726F (* 'wgro' *);
	kThemeDragSoundMoveUtilWindow = $756D6F76 (* 'umov' *);
	kThemeDragSoundGrowUtilWindow = $7567726F (* 'ugro' *);
	kThemeDragSoundMoveDialog = $646D6F76 (* 'dmov' *);
	kThemeDragSoundMoveAlert = $616D6F76 (* 'amov' *);
	kThemeDragSoundMoveIcon = $696D6F76 (* 'imov' *);
	kThemeDragSoundSliderThumb = $736C7468 (* 'slth' *);
	kThemeDragSoundSliderGhost = $736C6768 (* 'slgh' *);
	kThemeDragSoundScrollBarThumb = $73627468 (* 'sbth' *);
	kThemeDragSoundScrollBarGhost = $73626768 (* 'sbgh' *);
	kThemeDragSoundScrollBarArrowDecreasing = $73626164 (* 'sbad' *);
	kThemeDragSoundScrollBarArrowIncreasing = $73626169 (* 'sbai' *);
	kThemeDragSoundDragging = $64726167 (* 'drag' *);

type
	ThemeDragSoundKind = OSType;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ State-change sounds                                                      }
{ State-change sounds are played asynchonously as a one-shot.              }
{ Call PlayThemeSound to play the sound.  The sound will play              }
{ asynchronously until complete, then stop automatically.                  }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
const
	kThemeSoundNone = 0;
	kThemeSoundMenuOpen = $6D6E756F (* 'mnuo' *); { menu sounds }
	kThemeSoundMenuClose = $6D6E7563 (* 'mnuc' *);
	kThemeSoundMenuItemHilite = $6D6E7569 (* 'mnui' *);
	kThemeSoundMenuItemRelease = $6D6E7573 (* 'mnus' *);
	kThemeSoundWindowClosePress = $77636C70 (* 'wclp' *); { window sounds }
	kThemeSoundWindowCloseEnter = $77636C65 (* 'wcle' *);
	kThemeSoundWindowCloseExit = $77636C78 (* 'wclx' *);
	kThemeSoundWindowCloseRelease = $77636C72 (* 'wclr' *);
	kThemeSoundWindowZoomPress = $777A6D70 (* 'wzmp' *);
	kThemeSoundWindowZoomEnter = $777A6D65 (* 'wzme' *);
	kThemeSoundWindowZoomExit = $777A6D78 (* 'wzmx' *);
	kThemeSoundWindowZoomRelease = $777A6D72 (* 'wzmr' *);
	kThemeSoundWindowCollapsePress = $77636F70 (* 'wcop' *);
	kThemeSoundWindowCollapseEnter = $77636F65 (* 'wcoe' *);
	kThemeSoundWindowCollapseExit = $77636F78 (* 'wcox' *);
	kThemeSoundWindowCollapseRelease = $77636F72 (* 'wcor' *);
	kThemeSoundWindowDragBoundary = $77646264 (* 'wdbd' *);
	kThemeSoundUtilWinClosePress = $75636C70 (* 'uclp' *); { utility window sounds }
	kThemeSoundUtilWinCloseEnter = $75636C65 (* 'ucle' *);
	kThemeSoundUtilWinCloseExit = $75636C78 (* 'uclx' *);
	kThemeSoundUtilWinCloseRelease = $75636C72 (* 'uclr' *);
	kThemeSoundUtilWinZoomPress = $757A6D70 (* 'uzmp' *);
	kThemeSoundUtilWinZoomEnter = $757A6D65 (* 'uzme' *);
	kThemeSoundUtilWinZoomExit = $757A6D78 (* 'uzmx' *);
	kThemeSoundUtilWinZoomRelease = $757A6D72 (* 'uzmr' *);
	kThemeSoundUtilWinCollapsePress = $75636F70 (* 'ucop' *);
	kThemeSoundUtilWinCollapseEnter = $75636F65 (* 'ucoe' *);
	kThemeSoundUtilWinCollapseExit = $75636F78 (* 'ucox' *);
	kThemeSoundUtilWinCollapseRelease = $75636F72 (* 'ucor' *);
	kThemeSoundUtilWinDragBoundary = $75646264 (* 'udbd' *);
	kThemeSoundWindowOpen = $776F706E (* 'wopn' *); { window close and zoom action }
	kThemeSoundWindowClose = $77636C73 (* 'wcls' *);
	kThemeSoundWindowZoomIn = $777A6D69 (* 'wzmi' *);
	kThemeSoundWindowZoomOut = $777A6D6F (* 'wzmo' *);
	kThemeSoundWindowCollapseUp = $77636F6C (* 'wcol' *);
	kThemeSoundWindowCollapseDown = $77657870 (* 'wexp' *);
	kThemeSoundWindowActivate = $77616374 (* 'wact' *);
	kThemeSoundUtilWindowOpen = $756F706E (* 'uopn' *);
	kThemeSoundUtilWindowClose = $75636C73 (* 'ucls' *);
	kThemeSoundUtilWindowZoomIn = $757A6D69 (* 'uzmi' *);
	kThemeSoundUtilWindowZoomOut = $757A6D6F (* 'uzmo' *);
	kThemeSoundUtilWindowCollapseUp = $75636F6C (* 'ucol' *);
	kThemeSoundUtilWindowCollapseDown = $75657870 (* 'uexp' *);
	kThemeSoundUtilWindowActivate = $75616374 (* 'uact' *);
	kThemeSoundDialogOpen = $646F706E (* 'dopn' *);
	kThemeSoundDialogClose = $646C6763 (* 'dlgc' *);
	kThemeSoundAlertOpen = $616F706E (* 'aopn' *);
	kThemeSoundAlertClose = $616C7463 (* 'altc' *);
	kThemeSoundPopupWindowOpen = $70776F70 (* 'pwop' *);
	kThemeSoundPopupWindowClose = $7077636C (* 'pwcl' *);
	kThemeSoundButtonPress = $62746E70 (* 'btnp' *); { button }
	kThemeSoundButtonEnter = $62746E65 (* 'btne' *);
	kThemeSoundButtonExit = $62746E78 (* 'btnx' *);
	kThemeSoundButtonRelease = $62746E72 (* 'btnr' *);
	kThemeSoundDefaultButtonPress = $64627470 (* 'dbtp' *); { default button }
	kThemeSoundDefaultButtonEnter = $64627465 (* 'dbte' *);
	kThemeSoundDefaultButtonExit = $64627478 (* 'dbtx' *);
	kThemeSoundDefaultButtonRelease = $64627472 (* 'dbtr' *);
	kThemeSoundCancelButtonPress = $63627470 (* 'cbtp' *); { cancel button }
	kThemeSoundCancelButtonEnter = $63627465 (* 'cbte' *);
	kThemeSoundCancelButtonExit = $63627478 (* 'cbtx' *);
	kThemeSoundCancelButtonRelease = $63627472 (* 'cbtr' *);
	kThemeSoundCheckboxPress = $63686B70 (* 'chkp' *); { checkboxes }
	kThemeSoundCheckboxEnter = $63686B65 (* 'chke' *);
	kThemeSoundCheckboxExit = $63686B78 (* 'chkx' *);
	kThemeSoundCheckboxRelease = $63686B72 (* 'chkr' *);
	kThemeSoundRadioPress = $72616470 (* 'radp' *); { radio buttons }
	kThemeSoundRadioEnter = $72616465 (* 'rade' *);
	kThemeSoundRadioExit = $72616478 (* 'radx' *);
	kThemeSoundRadioRelease = $72616472 (* 'radr' *);
	kThemeSoundScrollArrowPress = $73626170 (* 'sbap' *); { scroll bars }
	kThemeSoundScrollArrowEnter = $73626165 (* 'sbae' *);
	kThemeSoundScrollArrowExit = $73626178 (* 'sbax' *);
	kThemeSoundScrollArrowRelease = $73626172 (* 'sbar' *);
	kThemeSoundScrollEndOfTrack = $73627465 (* 'sbte' *);
	kThemeSoundScrollTrackPress = $73627470 (* 'sbtp' *);
	kThemeSoundSliderEndOfTrack = $736C7465 (* 'slte' *); { sliders }
	kThemeSoundSliderTrackPress = $736C7470 (* 'sltp' *);
	kThemeSoundBalloonOpen = $626C6E6F (* 'blno' *); { help balloons }
	kThemeSoundBalloonClose = $626C6E63 (* 'blnc' *);
	kThemeSoundBevelPress = $62657670 (* 'bevp' *); { Bevel buttons }
	kThemeSoundBevelEnter = $62657665 (* 'beve' *);
	kThemeSoundBevelExit = $62657678 (* 'bevx' *);
	kThemeSoundBevelRelease = $62657672 (* 'bevr' *);
	kThemeSoundLittleArrowUpPress = $6C617570 (* 'laup' *); { Little Arrows }
	kThemeSoundLittleArrowDnPress = $6C616470 (* 'ladp' *);
	kThemeSoundLittleArrowEnter = $6C617265 (* 'lare' *);
	kThemeSoundLittleArrowExit = $6C617278 (* 'larx' *);
	kThemeSoundLittleArrowUpRelease = $6C617572 (* 'laur' *);
	kThemeSoundLittleArrowDnRelease = $6C616472 (* 'ladr' *);
	kThemeSoundPopupPress = $706F7070 (* 'popp' *); { Popup Buttons }
	kThemeSoundPopupEnter = $706F7065 (* 'pope' *);
	kThemeSoundPopupExit = $706F7078 (* 'popx' *);
	kThemeSoundPopupRelease = $706F7072 (* 'popr' *);
	kThemeSoundDisclosurePress = $64736370 (* 'dscp' *); { Disclosure Buttons }
	kThemeSoundDisclosureEnter = $64736365 (* 'dsce' *);
	kThemeSoundDisclosureExit = $64736378 (* 'dscx' *);
	kThemeSoundDisclosureRelease = $64736372 (* 'dscr' *);
	kThemeSoundTabPressed = $74616270 (* 'tabp' *); { Tabs }
	kThemeSoundTabEnter = $74616265 (* 'tabe' *);
	kThemeSoundTabExit = $74616278 (* 'tabx' *);
	kThemeSoundTabRelease = $74616272 (* 'tabr' *);
	kThemeSoundDragTargetHilite = $64746869 (* 'dthi' *); { drag manager }
	kThemeSoundDragTargetUnhilite = $64747568 (* 'dtuh' *);
	kThemeSoundDragTargetDrop = $64746472 (* 'dtdr' *);
	kThemeSoundEmptyTrash = $66747273 (* 'ftrs' *); { finder }
	kThemeSoundSelectItem = $6673656C (* 'fsel' *);
	kThemeSoundNewItem = $666E6577 (* 'fnew' *);
	kThemeSoundReceiveDrop = $66647270 (* 'fdrp' *);
	kThemeSoundCopyDone = $66637064 (* 'fcpd' *);
	kThemeSoundResolveAlias = $6672616C (* 'fral' *);
	kThemeSoundLaunchApp = $666C6170 (* 'flap' *);
	kThemeSoundDiskInsert = $64736B69 (* 'dski' *);
	kThemeSoundDiskEject = $64736B65 (* 'dske' *);
	kThemeSoundFinderDragOnIcon = $66646F6E (* 'fdon' *);
	kThemeSoundFinderDragOffIcon = $66646F66 (* 'fdof' *);

type
	ThemeSoundKind = OSType;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Window Metrics                                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  Window metrics are used by the Appearance manager to fill in the blanks necessary to    }
{  draw windows. If a value is not appropriate for the type of window, be sure to fill in  }
{  the slot in the structure with zero.    For the popupTabOffset parameter, you can pass a}
{  real value based on the left edge of the window. This value might be interpreted in a   }
{  different manner when depending on the value of the popupTabPosition field. The values  }
{  you can pass into popupTabPosition are:                                                 }
{  kThemePopupTabNormalPosition                                                            }
{      Starts the tab left edge at the position indicated by the popupTabOffset field.     }
{  kThemePopupTabCenterOnWindow                                                            }
{      tells us to ignore the offset field and instead simply center the width of the      }
{      handle on the window.                                                               }
{  kThemePopupTabCenterOnOffset                                                            }
{      tells us to center the width of the handle around the value passed in offset.       }
{  The Appearance Manager will try its best to accomodate the requested placement, but may }
{  move the handle slightly to make it fit correctly.                                      }
const
	kThemePopupTabNormalPosition = 0;
	kThemePopupTabCenterOnWindow = 1;
	kThemePopupTabCenterOnOffset = 2;

type
	ThemeWindowMetrics = record
		metricSize: UInt16;             { should be always be sizeof( ThemeWindowMetrics )}
		titleHeight: SInt16;
		titleWidth: SInt16;
		popupTabOffset: SInt16;
		popupTabWidth: SInt16;
		popupTabPosition: UInt16;
	end;
type
	ThemeWindowMetricsPtr = ^ThemeWindowMetrics;

{
 *  Summary:
 *    Theme metrics allow you to find out sizes of things in the
 *    current environment, such as how wide a scroll bar is, etc.
 *  
 *  Discussion:
 *    ThemeMetrics
 }
const
{
   * The width (or height if horizontal) of a scroll bar.
   }
	kThemeMetricScrollBarWidth = 0;

  {
   * The width (or height if horizontal) of a small scroll bar.
   }
	kThemeMetricSmallScrollBarWidth = 1;

  {
   * The height of the non-label part of a check box control.
   }
	kThemeMetricCheckBoxHeight = 2;

  {
   * The height of the non-label part of a radio button control.
   }
	kThemeMetricRadioButtonHeight = 3;

  {
   * The amount of white space surrounding the text Rect of the text
   * inside of an Edit Text control.  If you select all of the text in
   * an Edit Text control, you can see the white space. The metric is
   * the number of pixels, per side, that the text Rect is outset to
   * create the whitespace Rect.
   }
	kThemeMetricEditTextWhitespace = 4;

  {
   * The thickness of the Edit Text frame that surrounds the whitespace
   * Rect (that is surrounding the text Rect). The metric is the number
   * of pixels, per side, that the frame Rect is outset from the
   * whitespace Rect.
   }
	kThemeMetricEditTextFrameOutset = 5;

  {
   * The number of pixels that the list box frame is outset from the
   * content of the list box.
   }
	kThemeMetricListBoxFrameOutset = 6;

  {
   * This is a deprecated metric.  Don't use it.  It used to describe
   * how far the focus rect used to draw from a control, but control
   * focus drawing no longer uses this information to draw its focus.
   }
	kThemeMetricFocusRectOutset = 7;

  {
   * The thickness of the frame drawn by DrawThemeGenericWell.
   }
	kThemeMetricImageWellThickness = 8;

  {
   * The number of pixels a scrollbar should overlap (actually
   * underlap) any bounding box which surrounds it and scrollable
   * content. This also includes the window frame when a scrolbar is
   * along an edge of the window.
   }
	kThemeMetricScrollBarOverlap = 9;

  {
   * The height of the large tab of a tab control.
   }
	kThemeMetricLargeTabHeight = 10;

  {
   * The width of the caps (end pieces) of the large tabs of a tab
   * control.
   }
	kThemeMetricLargeTabCapsWidth = 11;

  {
   * The amount to add to the tab height (kThemeMetricLargeTabHeight)
   * to find out the rectangle height to use with the various Tab
   * drawing primitives. This amount is also the amount that each tab
   * overlaps the tab pane.
   }
	kThemeMetricTabFrameOverlap = 12;

  {
   * If less than zero, this indicates that the text should be centered
   * on each tab. If greater than zero, the text should be justified
   * (according to the system script direction) and the amount is the
   * offset from the appropriate edge at which the text should start
   * drawing.
   }
	kThemeMetricTabIndentOrStyle = 13;

  {
   * The amount of space that every tab's drawing rectangle overlaps
   * the one on either side of it.
   }
	kThemeMetricTabOverlap = 14;

  {
   * The height of the small tab of a tab control.  This includes the
   * pixels that overlap the tab pane and/or tab pane bar.
   }
	kThemeMetricSmallTabHeight = 15;

  {
   * The width of the caps (end pieces) of the small tabs of a tab
   * control.
   }
	kThemeMetricSmallTabCapsWidth = 16;

  {
   * The height and the width of the push button control.
   }
	kThemeMetricPushButtonHeight = 19;

  {
   * The height of the list header field of the data browser control.
   }
	kThemeMetricListHeaderHeight = 20;

  {
   * The height of a disclosure triangle control.  This triangle is the
   * not the center of the disclosure button, but its own control.
   }
	kThemeMetricDisclosureTriangleHeight = 25;

  {
   * The width of a disclosure triangle control.
   }
	kThemeMetricDisclosureTriangleWidth = 26;

  {
   * The height of a little arrows control.
   }
	kThemeMetricLittleArrowsHeight = 27;

  {
   * The width of a little arrows control.
   }
	kThemeMetricLittleArrowsWidth = 28;

  {
   * The height of a popup button control.
   }
	kThemeMetricPopupButtonHeight = 30;

  {
   * The height of a small popup button control.
   }
	kThemeMetricSmallPopupButtonHeight = 31;

  {
   * The height of the large progress bar, not including its shadow.
   }
	kThemeMetricLargeProgressBarThickness = 32;

  {
   * This metric is not used.
   }
	kThemeMetricPullDownHeight = 33;

  {
   * This metric is not used.
   }
	kThemeMetricSmallPullDownHeight = 34;

  {
   * The height of the window grow box control.
   }
	kThemeMetricResizeControlHeight = 38;

  {
   * The width of the window grow box control.
   }
	kThemeMetricSmallResizeControlHeight = 39;

  {
   * The height of the horizontal slider control.
   }
	kThemeMetricHSliderHeight = 41;

  {
   * The height of the tick marks for a horizontal slider control.
   }
	kThemeMetricHSliderTickHeight = 42;

  {
   * The width of the vertical slider control.
   }
	kThemeMetricVSliderWidth = 45;

  {
   * The width of the tick marks for a vertical slider control.
   }
	kThemeMetricVSliderTickWidth = 46;

  {
   * The height of the title bar widgets (grow, close, and zoom boxes)
   * for a document window.
   }
	kThemeMetricTitleBarControlsHeight = 49;

  {
   * The width of the non-label part of a check box control.
   }
	kThemeMetricCheckBoxWidth = 50;

  {
   * The width of the non-label part of a radio button control.
   }
	kThemeMetricRadioButtonWidth = 52;

  {
   * The height of the normal bar, not including its shadow.
   }
	kThemeMetricNormalProgressBarThickness = 58;

  {
   * The number of pixels of shadow depth drawn below the progress bar.
   }
	kThemeMetricProgressBarShadowOutset = 59;

  {
   * The number of pixels of shadow depth drawn below the small
   * progress bar.
   }
	kThemeMetricSmallProgressBarShadowOutset = 60;

  {
   * The number of pixels that the content of a primary group box is
   * from the bounds of the control.
   }
	kThemeMetricPrimaryGroupBoxContentInset = 61;

  {
   * The number of pixels that the content of a secondary group box is
   * from the bounds of the control.
   }
	kThemeMetricSecondaryGroupBoxContentInset = 62;

  {
   * Width allocated to draw the mark character in a menu.
   }
	kThemeMetricMenuMarkColumnWidth = 63;

  {
   * Width allocated for the mark character in a menu item when the
   * menu has kMenuAttrExcludesMarkColumn.
   }
	kThemeMetricMenuExcludedMarkColumnWidth = 64;

  {
   * Indent into the interior of the mark column at which the mark
   * character is drawn.
   }
	kThemeMetricMenuMarkIndent = 65;

  {
   * Whitespace at the leading edge of menu item text.
   }
	kThemeMetricMenuTextLeadingEdgeMargin = 66;

  {
   * Whitespace at the trailing edge of menu item text.
   }
	kThemeMetricMenuTextTrailingEdgeMargin = 67;

  {
   * Width per indent level (set by SetMenuItemIndent) of a menu item.
   }
	kThemeMetricMenuIndentWidth = 68;

  {
   * Whitespace at the trailing edge of a menu icon (if the item also
   * has text).
   }
	kThemeMetricMenuIconTrailingEdgeMargin = 69;


{
 *  Discussion:
 *    The following metrics are only available in OS X.
 }
const
{
   * The height of a disclosure button.
   }
	kThemeMetricDisclosureButtonHeight = 17;

  {
   * The height and the width of the round button control.
   }
	kThemeMetricRoundButtonSize = 18;

  {
   * The height of the non-label part of a small check box control.
   }
	kThemeMetricSmallCheckBoxHeight = 21;

  {
   * The width of a disclosure button.
   }
	kThemeMetricDisclosureButtonWidth = 22;

  {
   * The height of a small disclosure button.
   }
	kThemeMetricSmallDisclosureButtonHeight = 23;

  {
   * The width of a small disclosure button.
   }
	kThemeMetricSmallDisclosureButtonWidth = 24;

  {
   * The height (or width if vertical) of a pane splitter.
   }
	kThemeMetricPaneSplitterHeight = 29;

  {
   * The height of the small push button control.
   }
	kThemeMetricSmallPushButtonHeight = 35;

  {
   * The height of the non-label part of a small radio button control.
   }
	kThemeMetricSmallRadioButtonHeight = 36;

  {
   * The height of the relevance indicator control.
   }
	kThemeMetricRelevanceIndicatorHeight = 37;

  {
   * The height and the width of the large round button control.
   }
	kThemeMetricLargeRoundButtonSize = 40;

  {
   * The height of the small, horizontal slider control.
   }
	kThemeMetricSmallHSliderHeight = 43;

  {
   * The height of the tick marks for a small, horizontal slider
   * control.
   }
	kThemeMetricSmallHSliderTickHeight = 44;

  {
   * The width of the small, vertical slider control.
   }
	kThemeMetricSmallVSliderWidth = 47;

  {
   * The width of the tick marks for a small, vertical slider control.
   }
	kThemeMetricSmallVSliderTickWidth = 48;

  {
   * The width of the non-label part of a small check box control.
   }
	kThemeMetricSmallCheckBoxWidth = 51;

  {
   * The width of the non-label part of a small radio button control.
   }
	kThemeMetricSmallRadioButtonWidth = 53;

  {
   * The minimum width of the thumb of a small, horizontal slider
   * control.
   }
	kThemeMetricSmallHSliderMinThumbWidth = 54;

  {
   * The minimum width of the thumb of a small, vertical slider control.
   }
	kThemeMetricSmallVSliderMinThumbHeight = 55;

  {
   * The offset of the tick marks from the appropriate side of a small
   * horizontal slider control.
   }
	kThemeMetricSmallHSliderTickOffset = 56;

  {
   * The offset of the tick marks from the appropriate side of a small
   * vertical slider control.
   }
	kThemeMetricSmallVSliderTickOffset = 57;


{
 *  Discussion:
 *    The following metrics are only available in Mac OS X 10.3 and
 *    later.
 }
const
	kThemeMetricComboBoxLargeBottomShadowOffset = 70;
	kThemeMetricComboBoxLargeRightShadowOffset = 71;
	kThemeMetricComboBoxSmallBottomShadowOffset = 72;
	kThemeMetricComboBoxSmallRightShadowOffset = 73;
	kThemeMetricComboBoxLargeDisclosureWidth = 74;
	kThemeMetricComboBoxSmallDisclosureWidth = 75;
	kThemeMetricRoundTextFieldContentInsetLeft = 76;
	kThemeMetricRoundTextFieldContentInsetRight = 77;
	kThemeMetricRoundTextFieldContentInsetBottom = 78;
	kThemeMetricRoundTextFieldContentInsetTop = 79;
	kThemeMetricRoundTextFieldContentHeight = 80;
	kThemeMetricComboBoxMiniBottomShadowOffset = 81;
	kThemeMetricComboBoxMiniDisclosureWidth = 82;
	kThemeMetricComboBoxMiniRightShadowOffset = 83;
	kThemeMetricLittleArrowsMiniHeight = 84;
	kThemeMetricLittleArrowsMiniWidth = 85;
	kThemeMetricLittleArrowsSmallHeight = 86;
	kThemeMetricLittleArrowsSmallWidth = 87;
	kThemeMetricMiniCheckBoxHeight = 88;
	kThemeMetricMiniCheckBoxWidth = 89;
	kThemeMetricMiniDisclosureButtonHeight = 90;
	kThemeMetricMiniDisclosureButtonWidth = 91;
	kThemeMetricMiniHSliderHeight = 92;
	kThemeMetricMiniHSliderMinThumbWidth = 93;
	kThemeMetricMiniHSliderTickHeight = 94;
	kThemeMetricMiniHSliderTickOffset = 95;
	kThemeMetricMiniPopupButtonHeight = 96;
	kThemeMetricMiniPullDownHeight = 97;
	kThemeMetricMiniPushButtonHeight = 98;
	kThemeMetricMiniRadioButtonHeight = 99;
	kThemeMetricMiniRadioButtonWidth = 100;
	kThemeMetricMiniTabCapsWidth = 101;
	kThemeMetricMiniTabFrameOverlap = 102;
	kThemeMetricMiniTabHeight = 103;
	kThemeMetricMiniTabOverlap = 104;
	kThemeMetricMiniVSliderMinThumbHeight = 105;
	kThemeMetricMiniVSliderTickOffset = 106;
	kThemeMetricMiniVSliderTickWidth = 107;
	kThemeMetricMiniVSliderWidth = 108;
	kThemeMetricRoundTextFieldContentInsetWithIconLeft = 109;
	kThemeMetricRoundTextFieldContentInsetWithIconRight = 110;
	kThemeMetricRoundTextFieldMiniContentHeight = 111;
	kThemeMetricRoundTextFieldMiniContentInsetBottom = 112;
	kThemeMetricRoundTextFieldMiniContentInsetLeft = 113;
	kThemeMetricRoundTextFieldMiniContentInsetRight = 114;
	kThemeMetricRoundTextFieldMiniContentInsetTop = 115;
	kThemeMetricRoundTextFieldMiniContentInsetWithIconLeft = 116;
	kThemeMetricRoundTextFieldMiniContentInsetWithIconRight = 117;
	kThemeMetricRoundTextFieldSmallContentHeight = 118;
	kThemeMetricRoundTextFieldSmallContentInsetBottom = 119;
	kThemeMetricRoundTextFieldSmallContentInsetLeft = 120;
	kThemeMetricRoundTextFieldSmallContentInsetRight = 121;
	kThemeMetricRoundTextFieldSmallContentInsetTop = 122;
	kThemeMetricRoundTextFieldSmallContentInsetWithIconLeft = 123;
	kThemeMetricRoundTextFieldSmallContentInsetWithIconRight = 124;
	kThemeMetricSmallTabFrameOverlap = 125;
	kThemeMetricSmallTabOverlap = 126;

  {
   * The height of a small pane splitter. Should only be used in a
   * window with thick borders, like a metal window.
   }
	kThemeMetricSmallPaneSplitterHeight = 127;


{
 *  Discussion:
 *    The following metrics are only available in Mac OS X 10.4 and
 *    later.
 }
const
{
   * The horizontal start offset for the first tick mark on a
   * horizontal slider.
   }
	kThemeMetricHSliderTickOffset = 128;

  {
   * The vertical start offset for the first tick mark on a vertical
   * slider.
   }
	kThemeMetricVSliderTickOffset = 129;

  {
   * The minimum thumb height for a thumb on a slider.
   }
	kThemeMetricSliderMinThumbHeight = 130;
	kThemeMetricSliderMinThumbWidth = 131;

  {
   * The minimum thumb height for a thumb on a scroll bar.
   }
	kThemeMetricScrollBarMinThumbHeight = 132;

  {
   * The minimum thumb width for a thumb on a scroll bar.
   }
	kThemeMetricScrollBarMinThumbWidth = 133;

  {
   * The minimum thumb height for a thumb on a small scroll bar.
   }
	kThemeMetricSmallScrollBarMinThumbHeight = 134;

  {
   * The minimum thumb width for a thumb on a small scroll bar.
   }
	kThemeMetricSmallScrollBarMinThumbWidth = 135;


type
	ThemeMetric = UInt32;
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Drawing State                                                                            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
type
	ThemeDrawingState = ^SInt32; { an opaque 32-bit type }
	ThemeDrawingStatePtr = ^ThemeDrawingState;  { when a var xx:ThemeDrawingState parameter can be nil, it is changed to xx: ThemeDrawingStatePtr }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Callback procs                                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
type
	ThemeTabTitleDrawProcPtr = procedure( const (*var*) bounds: Rect; style: ThemeTabStyle; direction: ThemeTabDirection; depth: SInt16; isColorDev: Boolean; userData: UInt32 );
type
	ThemeEraseProcPtr = procedure( const (*var*) bounds: Rect; eraseData: UInt32; depth: SInt16; isColorDev: Boolean );
type
	ThemeButtonDrawProcPtr = procedure( const (*var*) bounds: Rect; kind: ThemeButtonKind; const (*var*) info: ThemeButtonDrawInfo; userData: UInt32; depth: SInt16; isColorDev: Boolean );
type
	WindowTitleDrawingProcPtr = procedure( const (*var*) bounds: Rect; depth: SInt16; colorDevice: Boolean; userData: UInt32 );
type
	ThemeIteratorProcPtr = function( const (*var*) inFileName: Str255; resID: SInt16; inThemeSettings: Collection; inUserData: UnivPtr ): Boolean;
type
	ThemeTabTitleDrawUPP = ThemeTabTitleDrawProcPtr;
type
	ThemeEraseUPP = ThemeEraseProcPtr;
type
	ThemeButtonDrawUPP = ThemeButtonDrawProcPtr;
type
	WindowTitleDrawingUPP = WindowTitleDrawingProcPtr;
type
	ThemeIteratorUPP = ThemeIteratorProcPtr;
{
 *  NewThemeTabTitleDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThemeTabTitleDrawUPP( userRoutine: ThemeTabTitleDrawProcPtr ): ThemeTabTitleDrawUPP; external name '_NewThemeTabTitleDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThemeEraseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThemeEraseUPP( userRoutine: ThemeEraseProcPtr ): ThemeEraseUPP; external name '_NewThemeEraseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThemeButtonDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThemeButtonDrawUPP( userRoutine: ThemeButtonDrawProcPtr ): ThemeButtonDrawUPP; external name '_NewThemeButtonDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewWindowTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewWindowTitleDrawingUPP( userRoutine: WindowTitleDrawingProcPtr ): WindowTitleDrawingUPP; external name '_NewWindowTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewThemeIteratorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewThemeIteratorUPP( userRoutine: ThemeIteratorProcPtr ): ThemeIteratorUPP; external name '_NewThemeIteratorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThemeTabTitleDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThemeTabTitleDrawUPP( userUPP: ThemeTabTitleDrawUPP ); external name '_DisposeThemeTabTitleDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThemeEraseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThemeEraseUPP( userUPP: ThemeEraseUPP ); external name '_DisposeThemeEraseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThemeButtonDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThemeButtonDrawUPP( userUPP: ThemeButtonDrawUPP ); external name '_DisposeThemeButtonDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeWindowTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeWindowTitleDrawingUPP( userUPP: WindowTitleDrawingUPP ); external name '_DisposeWindowTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeThemeIteratorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeThemeIteratorUPP( userUPP: ThemeIteratorUPP ); external name '_DisposeThemeIteratorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThemeTabTitleDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeThemeTabTitleDrawUPP( const (*var*) bounds: Rect; style: ThemeTabStyle; direction: ThemeTabDirection; depth: SInt16; isColorDev: Boolean; userData: UInt32; userUPP: ThemeTabTitleDrawUPP ); external name '_InvokeThemeTabTitleDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThemeEraseUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeThemeEraseUPP( const (*var*) bounds: Rect; eraseData: UInt32; depth: SInt16; isColorDev: Boolean; userUPP: ThemeEraseUPP ); external name '_InvokeThemeEraseUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThemeButtonDrawUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeThemeButtonDrawUPP( const (*var*) bounds: Rect; kind: ThemeButtonKind; const (*var*) info: ThemeButtonDrawInfo; userData: UInt32; depth: SInt16; isColorDev: Boolean; userUPP: ThemeButtonDrawUPP ); external name '_InvokeThemeButtonDrawUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeWindowTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeWindowTitleDrawingUPP( const (*var*) bounds: Rect; depth: SInt16; colorDevice: Boolean; userData: UInt32; userUPP: WindowTitleDrawingUPP ); external name '_InvokeWindowTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeThemeIteratorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeThemeIteratorUPP( const (*var*) inFileName: Str255; resID: SInt16; inThemeSettings: Collection; inUserData: UnivPtr; userUPP: ThemeIteratorUPP ): Boolean; external name '_InvokeThemeIteratorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Menu Drawing callbacks                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
type
	MenuTitleDrawingProcPtr = procedure( const (*var*) inBounds: Rect; inDepth: SInt16; inIsColorDevice: Boolean; inUserData: SInt32 );
type
	MenuItemDrawingProcPtr = procedure( const (*var*) inBounds: Rect; inDepth: SInt16; inIsColorDevice: Boolean; inUserData: SInt32 );
type
	MenuTitleDrawingUPP = MenuTitleDrawingProcPtr;
type
	MenuItemDrawingUPP = MenuItemDrawingProcPtr;
{
 *  NewMenuTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewMenuTitleDrawingUPP( userRoutine: MenuTitleDrawingProcPtr ): MenuTitleDrawingUPP; external name '_NewMenuTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewMenuItemDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewMenuItemDrawingUPP( userRoutine: MenuItemDrawingProcPtr ): MenuItemDrawingUPP; external name '_NewMenuItemDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeMenuTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeMenuTitleDrawingUPP( userUPP: MenuTitleDrawingUPP ); external name '_DisposeMenuTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeMenuItemDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeMenuItemDrawingUPP( userUPP: MenuItemDrawingUPP ); external name '_DisposeMenuItemDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeMenuTitleDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeMenuTitleDrawingUPP( const (*var*) inBounds: Rect; inDepth: SInt16; inIsColorDevice: Boolean; inUserData: SInt32; userUPP: MenuTitleDrawingUPP ); external name '_InvokeMenuTitleDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeMenuItemDrawingUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeMenuItemDrawingUPP( const (*var*) inBounds: Rect; inDepth: SInt16; inIsColorDevice: Boolean; inUserData: SInt32; userUPP: MenuItemDrawingUPP ); external name '_InvokeMenuItemDrawingUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  Appearance Manager APIs                                                         }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Registering Appearance-Savvy Applications }
{
 *  RegisterAppearanceClient()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function RegisterAppearanceClient: OSStatus; external name '_RegisterAppearanceClient';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UnregisterAppearanceClient()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function UnregisterAppearanceClient: OSStatus; external name '_UnregisterAppearanceClient';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  IsAppearanceClient()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function IsAppearanceClient( const (*var*) process: ProcessSerialNumber ): Boolean; external name '_IsAppearanceClient';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{****************************************************************************
    NOTES ON THEME BRUSHES
    Theme brushes can be either colors or patterns, depending on the theme.
    Because of this, you should be prepared to handle the case where a brush
    is a pattern and save and restore the pnPixPat and bkPixPat fields of
    your GrafPorts when saving the fore and back colors. Also, since patterns
    in bkPixPat override the background color of the window, you should use
    BackPat to set your background pattern to a normal white pattern. This
    will ensure that you can use RGBBackColor to set your back color to white,
    call EraseRect and get the expected results.
****************************************************************************}

{
 *  SetThemePen()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function SetThemePen( inBrush: ThemeBrush; inDepth: SInt16; inIsColorDevice: Boolean ): OSStatus; external name '_SetThemePen';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThemeBackground()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function SetThemeBackground( inBrush: ThemeBrush; inDepth: SInt16; inIsColorDevice: Boolean ): OSStatus; external name '_SetThemeBackground';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThemeTextColor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function SetThemeTextColor( inColor: ThemeTextColor; inDepth: SInt16; inIsColorDevice: Boolean ): OSStatus; external name '_SetThemeTextColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThemeWindowBackground() has moved to MacWindows.h
 }
{ Window Placards, Headers and Frames }
{
 *  DrawThemeWindowHeader()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeWindowHeader( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeWindowHeader';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeWindowListViewHeader()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeWindowListViewHeader( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeWindowListViewHeader';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemePlacard()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemePlacard( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemePlacard';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeEditTextFrame()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeEditTextFrame( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeEditTextFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeListBoxFrame()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeListBoxFrame( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeListBoxFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Keyboard Focus Drawing }
{
 *  DrawThemeFocusRect()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeFocusRect( const (*var*) inRect: Rect; inHasFocus: Boolean ): OSStatus; external name '_DrawThemeFocusRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Dialog Group Boxes and Separators }
{
 *  DrawThemePrimaryGroup()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemePrimaryGroup( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemePrimaryGroup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeSecondaryGroup()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeSecondaryGroup( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeSecondaryGroup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeSeparator()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeSeparator( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeSeparator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ BEGIN APPEARANCE 1.0.1 ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ The following Appearance Manager APIs are only available }
{ in Appearance 1.0.1 or later                             }
{
 *  DrawThemeModelessDialogFrame()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeModelessDialogFrame( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeModelessDialogFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeGenericWell()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeGenericWell( const (*var*) inRect: Rect; inState: ThemeDrawState; inFillCenter: Boolean ): OSStatus; external name '_DrawThemeGenericWell';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeFocusRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeFocusRegion( inRegion: RgnHandle; inHasFocus: Boolean ): OSStatus; external name '_DrawThemeFocusRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  IsThemeInColor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function IsThemeInColor( inDepth: SInt16; inIsColorDevice: Boolean ): Boolean; external name '_IsThemeInColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ IMPORTANT: GetThemeAccentColors will only work in the platinum theme. Any other theme will }
{ most likely return an error }
{
 *  GetThemeAccentColors()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeAccentColors( var outColors: CTabHandle ): OSStatus; external name '_GetThemeAccentColors';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeMenuBarBackground()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeMenuBarBackground( const (*var*) inBounds: Rect; inState: ThemeMenuBarState; inAttributes: UInt32 ): OSStatus; external name '_DrawThemeMenuBarBackground';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeMenuTitle()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeMenuTitle( const (*var*) inMenuBarRect: Rect; const (*var*) inTitleRect: Rect; inState: ThemeMenuState; inAttributes: UInt32; inTitleProc: MenuTitleDrawingUPP { can be NULL }; inTitleData: UInt32 ): OSStatus; external name '_DrawThemeMenuTitle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeMenuBarHeight()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeMenuBarHeight( var outHeight: SInt16 ): OSStatus; external name '_GetThemeMenuBarHeight';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeMenuBackground()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeMenuBackground( const (*var*) inMenuRect: Rect; inMenuType: ThemeMenuType ): OSStatus; external name '_DrawThemeMenuBackground';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeMenuBackgroundRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeMenuBackgroundRegion( const (*var*) inMenuRect: Rect; menuType: ThemeMenuType; region: RgnHandle ): OSStatus; external name '_GetThemeMenuBackgroundRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeMenuItem()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeMenuItem( const (*var*) inMenuRect: Rect; const (*var*) inItemRect: Rect; inVirtualMenuTop: SInt16; inVirtualMenuBottom: SInt16; inState: ThemeMenuState; inItemType: ThemeMenuItemType; inDrawProc: MenuItemDrawingUPP { can be NULL }; inUserData: UInt32 ): OSStatus; external name '_DrawThemeMenuItem';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeMenuSeparator()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function DrawThemeMenuSeparator( const (*var*) inItemRect: Rect ): OSStatus; external name '_DrawThemeMenuSeparator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeMenuSeparatorHeight()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeMenuSeparatorHeight( var outHeight: SInt16 ): OSStatus; external name '_GetThemeMenuSeparatorHeight';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeMenuItemExtra()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeMenuItemExtra( inItemType: ThemeMenuItemType; var outHeight: SInt16; var outWidth: SInt16 ): OSStatus; external name '_GetThemeMenuItemExtra';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeMenuTitleExtra()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.0 and later
 }
function GetThemeMenuTitleExtra( var outWidth: SInt16; inIsSquished: Boolean ): OSStatus; external name '_GetThemeMenuTitleExtra';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ BEGIN APPEARANCE 1.1 ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ THEME SWITCHING ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  X ALERT: Please note that Get/SetTheme are severely neutered under Mac OS X at present.         }
{           See the note above regarding what collection tags are supported under X.               }

{
 *  GetTheme()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetTheme( ioCollection: Collection ): OSStatus; external name '_GetTheme';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetTheme()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function SetTheme( ioCollection: Collection ): OSStatus; external name '_SetTheme';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  IterateThemes()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function IterateThemes( inProc: ThemeIteratorUPP; inUserData: UnivPtr { can be NULL } ): OSStatus; external name '_IterateThemes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ TABS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  DrawThemeTabPane()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTabPane( const (*var*) inRect: Rect; inState: ThemeDrawState ): OSStatus; external name '_DrawThemeTabPane';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeTab()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTab( const (*var*) inRect: Rect; inStyle: ThemeTabStyle; inDirection: ThemeTabDirection; labelProc: ThemeTabTitleDrawUPP { can be NULL }; userData: UInt32 ): OSStatus; external name '_DrawThemeTab';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTabRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTabRegion( const (*var*) inRect: Rect; inStyle: ThemeTabStyle; inDirection: ThemeTabDirection; ioRgn: RgnHandle ): OSStatus; external name '_GetThemeTabRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ CURSORS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  SetThemeCursor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function SetThemeCursor( inCursor: ThemeCursor ): OSStatus; external name '_SetThemeCursor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetAnimatedThemeCursor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function SetAnimatedThemeCursor( inCursor: ThemeCursor; inAnimationStep: UInt32 ): OSStatus; external name '_SetAnimatedThemeCursor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ CONTROL STYLE SETTINGS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  GetThemeScrollBarThumbStyle()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeScrollBarThumbStyle( var outStyle: ThemeScrollBarThumbStyle ): OSStatus; external name '_GetThemeScrollBarThumbStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeScrollBarArrowStyle()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeScrollBarArrowStyle( var outStyle: ThemeScrollBarArrowStyle ): OSStatus; external name '_GetThemeScrollBarArrowStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeCheckBoxStyle()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeCheckBoxStyle( var outStyle: ThemeCheckBoxStyle ): OSStatus; external name '_GetThemeCheckBoxStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ FONTS/TEXT ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  UseThemeFont()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function UseThemeFont( inFontID: ThemeFontID; inScript: ScriptCode ): OSStatus; external name '_UseThemeFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeFont()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeFont( inFontID: ThemeFontID; inScript: ScriptCode; outFontName: StringPtr { can be NULL }; var outFontSize: SInt16; var outStyle: Style ): OSStatus; external name '_GetThemeFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeTextBox()
 *  
 *  Summary:
 *    Draws text into the area you specify.
 *  
 *  Discussion:
 *    DrawThemeTextBox allows you to draw theme-savvy (ie. Aqua-savvy
 *    on Mac OS X) text. It is unicode savvy (although only partially
 *    so under CarbonLib), and allows you to customize certain text
 *    rendering characteristics such as the font, wrapping behavior,
 *    and justification. The text is drawn into the CGContextRef you
 *    provide, or into the current Quickdraw port if no CGContextRef is
 *    provided. None of DrawThemeTextBox's parameters imply a color, so
 *    you must set up the desired text color separately before calling
 *    DrawThemeTextBox. If you provide a CGContextRef, its fill color
 *    will be used to draw the text. If you do not provide a
 *    CGContextRef, a color based on the current Quickdraw port's
 *    foreground color and the grayishTextOr mode (if set) will be used
 *    to draw the text.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inString:
 *      A CFStringRef containing the unicode characters you wish to
 *      render. You MUST NOT pass in a CFStringRef that was allocated
 *      with any of the "NoCopy" CFString creation APIs; a string
 *      created with a "NoCopy" API has transient storage which is
 *      incompatible with DrawThemeTextBox's caches.
 *    
 *    inFontID:
 *      The ThemeFontID describing the font you'd like to render the
 *      text with. See the discussion of ThemeFontIDs elsewhere in this
 *      header.
 *    
 *    inState:
 *      The ThemeDrawState describing the the state of the interface
 *      element you are drawing the text for. If, for example, you are
 *      drawing text for an inactive window, you would pass
 *      kThemeStateInactive. The ThemeDrawState is generally only used
 *      to determine the shadow characteristics for the text on Mac OS
 *      X. Note that the ThemeDrawState does NOT imply a color. It is
 *      NOT used as a mechanism for graying the text. If you wish to
 *      draw grayed text, you must set up the desired gray color and
 *      apply it to either the current Quickdraw port or the
 *      CGContextRef as appropriate.
 *    
 *    inWrapToWidth:
 *      A Boolean indicating whether you want to draw multiple lines of
 *      text wrapped to a bounding box. False indicates that only one
 *      line of text should be drawn without any sort of wrapping.
 *    
 *    inBoundingBox:
 *      The rectangle (in coordinates relative to the current Quickdraw
 *      port) describing the area to draw the text within. The first
 *      line of text will be top-justified to this rectangle. Wrapping
 *      (if desired) will happen at the horizontal extent of this
 *      rectangle. Regardless of the amount of text in your
 *      CFStringRef, all drawn text will be clipped to this rectangle.
 *    
 *    inJust:
 *      The horizontal justification you would like for your text. You
 *      can use one of the standard justification constants from
 *      TextEdit.h.
 *    
 *    inContext:
 *      The CGContextRef into which you would like to draw the text. On
 *      Mac OS X, all text drawing happens in CGContextRefs; if you
 *      pass NULL, a transient CGContextRef will be allocated and
 *      deallocated for use within the single API call. Relying on the
 *      system behavior if transiently creating CGContextRefs may
 *      result in performance problems. On Mac OS 9, the CGContextRef
 *      parameter is ignored.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   not available
 }
function DrawThemeTextBox( inString: CFStringRef; inFontID: ThemeFontID; inState: ThemeDrawState; inWrapToWidth: Boolean; const (*var*) inBoundingBox: Rect; inJust: SInt16; inContext: UnivPtr ): OSStatus; external name '_DrawThemeTextBox';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  TruncateThemeText()
 *  
 *  Summary:
 *    Truncates text to fit within the width you specify.
 *  
 *  Discussion:
 *    TruncateThemeText alters a unicode string to fit within a width
 *    that you specify. It is unicode savvy (although only partially so
 *    under CarbonLib), and makes its calculations (and any subsequent
 *    string alteration) based on the font and state you specify. If
 *    the string needs to be truncated, it will be reduced to the
 *    maximum number of characters which (with the addition of an
 *    ellipsis character) fits within the specified width.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inString:
 *      A CFMutableStringRef containing the unicode characters you wish
 *      to truncate. On output, inString may have been altered to fit
 *      within the specified width. You MUST NOT pass in a CFStringRef
 *      that was allocated with any of the "NoCopy" CFString creation
 *      APIs (see note in DrawThemeTextBox above).
 *    
 *    inFontID:
 *      The ThemeFontID to use for text measurements. See the
 *      discussion of ThemeFontIDs elsewhere in this header.
 *    
 *    inState:
 *      The ThemeDrawState which matches the state you will ultimately
 *      render the string width. This may affect text measurement
 *      during truncation, so you should be sure the value you pass to
 *      TruncateThemeText matches the value you will eventually use for
 *      drawing.
 *    
 *    inPixelWidthLimit:
 *      The maximum width (in pixels) that the resulting truncated
 *      string may have.
 *    
 *    inTruncWhere:
 *      A TruncCode indicating where you would like truncation to occur.
 *    
 *    outTruncated:
 *      On output, this Boolean value indicates whether the string was
 *      truncated. True means the string was truncated. False means the
 *      string was not (and did not need to be) truncated.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   not available
 }
function TruncateThemeText( inString: CFMutableStringRef; inFontID: ThemeFontID; inState: ThemeDrawState; inPixelWidthLimit: SInt16; inTruncWhere: TruncCode; outTruncated: BooleanPtr { can be NULL } ): OSStatus; external name '_TruncateThemeText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTextDimensions()
 *  
 *  Summary:
 *    Tells you the height, width, and baseline for a string.
 *  
 *  Discussion:
 *    GetThemeTextDimensions measures the given string using a font and
 *    state you specify. It always reports the actual height and
 *    baseline. It sometimes reports the actual width (see below). It
 *    can measure a string that wraps. It is unicode savvy (although
 *    only partially so under CarbonLib).
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inString:
 *      A CFStringRef containing the unicode characters you wish to
 *      measure. You MUST NOT pass in a CFStringRef that was allocated
 *      with any of the "NoCopy" CFString creation APIs (see note in
 *      DrawThemeTextBox above).
 *    
 *    inFontID:
 *      The ThemeFontID describing the font you'd like to measure the
 *      text with. See the discussion of ThemeFontIDs elsewhere in this
 *      header.
 *    
 *    inState:
 *      The ThemeDrawState which matches the state you will ultimately
 *      render the string width. This may affect text measurement, so
 *      you should be sure the value you pass to TruncateThemeText
 *      matches the value you will eventually use for drawing.
 *    
 *    inWrapToWidth:
 *      A Boolean indicating whether you want the measurements based on
 *      wrapping the text to a specific width. If you pass true, you
 *      must specify the desired width in ioBounds->h.
 *    
 *    ioBounds:
 *      On output, ioBounds->v contains the height of the text. If you
 *      pass false to inWrapToWidth, ioBounds->h will contain the width
 *      of the text on output. If you pass true to inWrapToWidth,
 *      ioBounds->h must (on input) contain the desired width for
 *      wrapping; on output, ioBounds->h contains the same value you
 *      specified on input.
 *    
 *    outBaseline:
 *      On output, outBaseline contains the offset (in Quickdraw space)
 *      from the bottom edge of the last line of text to the baseline
 *      of the first line of text. outBaseline will generally be a
 *      negative value. On Mac OS X 10.2 and later, you may pass NULL
 *      if you don't want this information.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   not available
 }
function GetThemeTextDimensions( inString: CFStringRef; inFontID: ThemeFontID; inState: ThemeDrawState; inWrapToWidth: Boolean; var ioBounds: Point; outBaseline: SInt16Ptr { can be NULL } ): OSStatus; external name '_GetThemeTextDimensions';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTextShadowOutset()
 *  
 *  Summary:
 *    Tells you the amount of space taken up by the shadow for a given
 *    font/state combination.
 *  
 *  Discussion:
 *    GetThemeTextShadowOutset passes back the maximum amount of space
 *    the shadow will take up for text drawn in the specified font and
 *    state. While GetThemeTextDimensions tells you how much space is
 *    taken up by the character glyphs themselves, it does not
 *    incorporate the font/state shadow into its calculations. If you
 *    need to know how much total space including the shadow will be
 *    taken up, call GetThemeTextDimensions followed by
 *    GetThemeTextShadowOutset.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inFontID:
 *      The ThemeFontID describing the font you'd like the shadow
 *      characteristics of. Font and state both determine the amount of
 *      shadow that will be used on rendered text. See the discussion
 *      of ThemeFontIDs elsewhere in this header.
 *    
 *    inState:
 *      The ThemeDrawState which matches the state you'd like the
 *      shadow characteristics of. Font and state both determine the
 *      amount of shadow that will be used on rendered text.
 *    
 *    outOutset:
 *      On output, outOutset contains the amount of space the shadow
 *      will take up beyond each edge of the text bounding rectangle
 *      returned by GetThemeTextDimensions. The fields of outOutset
 *      will either be positive values or zero.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   not available
 }
function GetThemeTextShadowOutset( inFontID: ThemeFontID; inState: ThemeDrawState; var outOutset: Rect ): OSStatus; external name '_GetThemeTextShadowOutset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ TRACKS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  DrawThemeTrack()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTrack( const (*var*) drawInfo: ThemeTrackDrawInfo; rgnGhost: RgnHandle { can be NULL }; eraseProc: ThemeEraseUPP { can be NULL }; eraseData: UInt32 ): OSStatus; external name '_DrawThemeTrack';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HitTestThemeTrack()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function HitTestThemeTrack( const (*var*) drawInfo: ThemeTrackDrawInfo; mousePoint: Point; var partHit: AppearancePartCode ): Boolean; external name '_HitTestThemeTrack';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackBounds()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackBounds( const (*var*) drawInfo: ThemeTrackDrawInfo; var bounds: Rect ): OSStatus; external name '_GetThemeTrackBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackThumbRgn()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackThumbRgn( const (*var*) drawInfo: ThemeTrackDrawInfo; thumbRgn: RgnHandle ): OSStatus; external name '_GetThemeTrackThumbRgn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackDragRect()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackDragRect( const (*var*) drawInfo: ThemeTrackDrawInfo; var dragRect: Rect ): OSStatus; external name '_GetThemeTrackDragRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeTrackTickMarks()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTrackTickMarks( const (*var*) drawInfo: ThemeTrackDrawInfo; numTicks: ItemCount; eraseProc: ThemeEraseUPP { can be NULL }; eraseData: UInt32 ): OSStatus; external name '_DrawThemeTrackTickMarks';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackThumbPositionFromOffset()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackThumbPositionFromOffset( const (*var*) drawInfo: ThemeTrackDrawInfo; thumbOffset: Point; var relativePosition: SInt32 ): OSStatus; external name '_GetThemeTrackThumbPositionFromOffset';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackThumbPositionFromRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackThumbPositionFromRegion( const (*var*) drawInfo: ThemeTrackDrawInfo; thumbRgn: RgnHandle; var relativePosition: SInt32 ): OSStatus; external name '_GetThemeTrackThumbPositionFromRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTrackLiveValue()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTrackLiveValue( const (*var*) drawInfo: ThemeTrackDrawInfo; relativePosition: SInt32; var value: SInt32 ): OSStatus; external name '_GetThemeTrackLiveValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ SCROLLBAR ARROWS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  DrawThemeScrollBarArrows()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeScrollBarArrows( const (*var*) bounds: Rect; enableState: ThemeTrackEnableState; pressState: ThemeTrackPressState; isHoriz: Boolean; var trackBounds: Rect ): OSStatus; external name '_DrawThemeScrollBarArrows';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeScrollBarTrackRect()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeScrollBarTrackRect( const (*var*) bounds: Rect; enableState: ThemeTrackEnableState; pressState: ThemeTrackPressState; isHoriz: Boolean; var trackBounds: Rect ): OSStatus; external name '_GetThemeScrollBarTrackRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HitTestThemeScrollBarArrows()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function HitTestThemeScrollBarArrows( const (*var*) scrollBarBounds: Rect; enableState: ThemeTrackEnableState; pressState: ThemeTrackPressState; isHoriz: Boolean; ptHit: Point; var trackBounds: Rect; var partcode: AppearancePartCode ): Boolean; external name '_HitTestThemeScrollBarArrows';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ WINDOWS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  GetThemeWindowRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeWindowRegion( flavor: ThemeWindowType; const (*var*) contRect: Rect; state: ThemeDrawState; const (*var*) metrics: ThemeWindowMetrics; attributes: ThemeWindowAttributes; winRegion: AppearanceRegionCode; rgn: RgnHandle ): OSStatus; external name '_GetThemeWindowRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeWindowFrame()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeWindowFrame( flavor: ThemeWindowType; const (*var*) contRect: Rect; state: ThemeDrawState; const (*var*) metrics: ThemeWindowMetrics; attributes: ThemeWindowAttributes; titleProc: WindowTitleDrawingUPP { can be NULL }; titleData: UInt32 ): OSStatus; external name '_DrawThemeWindowFrame';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeTitleBarWidget()
 *  
 *  Summary:
 *    Draws the requested theme title bar widget.
 *  
 *  Discussion:
 *    DrawThemeTitleBarWidget renders the requested theme title bar
 *    widget in the proper location of a window.  A common
 *    misconception when using this API is that the client must specify
 *    the exact location of the widget in the window. The widget will
 *    locate itself in the window based relative to the content rect
 *    passed in content rectangle -- the contRect parameter.  Another
 *    common problem is to ignore the window's attributes.  The
 *    attributes must be set up properly to describe the window for
 *    which the widget is to be drawn.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    flavor:
 *      A valid ThemeWindowtype describing the type of theme window for
 *      which you would like to draw a widget.
 *    
 *    contRect:
 *      A rectangle describing the window's content area.  The widget
 *      is drawn relative to the content rectangle of the window, so
 *      this parameter does not describe the actual widget bounds, it
 *      describes the window's content rectangle.
 *    
 *    state:
 *      A valid ThemeDrawState which describes the state of the window
 *      for which the widget is to be drawn.
 *    
 *    metrics:
 *      A pointer to a set of valid ThemeWindowMetrics.  At this time,
 *      none of the fields of the metrics are pertinent to the widgets,
 *      so the only important field is the metricSize field to mark the
 *      structure as valid.
 *    
 *    attributes:
 *      A valid ThemeWindowAttributes set which describes the window
 *      for which the widget is to be drawn.
 *    
 *    widget:
 *      A valid ThemeTitleBarWidget set which describes which widget to
 *      draw.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTitleBarWidget( flavor: ThemeWindowType; const (*var*) contRect: Rect; state: ThemeDrawState; const (*var*) metrics: ThemeWindowMetrics; attributes: ThemeWindowAttributes; widget: ThemeTitleBarWidget ): OSStatus; external name '_DrawThemeTitleBarWidget';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeWindowRegionHit()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeWindowRegionHit( flavor: ThemeWindowType; const (*var*) inContRect: Rect; state: ThemeDrawState; const (*var*) metrics: ThemeWindowMetrics; inAttributes: ThemeWindowAttributes; inPoint: Point; var outRegionHit: AppearanceRegionCode ): Boolean; external name '_GetThemeWindowRegionHit';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeScrollBarDelimiters()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeScrollBarDelimiters( flavor: ThemeWindowType; const (*var*) inContRect: Rect; state: ThemeDrawState; attributes: ThemeWindowAttributes ): OSStatus; external name '_DrawThemeScrollBarDelimiters';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ BUTTONS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  DrawThemeButton()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeButton( const (*var*) inBounds: Rect; inKind: ThemeButtonKind; const (*var*) inNewInfo: ThemeButtonDrawInfo; {const} inPrevInfo: ThemeButtonDrawInfoPtr { can be NULL }; inEraseProc: ThemeEraseUPP { can be NULL }; inLabelProc: ThemeButtonDrawUPP { can be NULL }; inUserData: UInt32 ): OSStatus; external name '_DrawThemeButton';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeButtonRegion()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeButtonRegion( const (*var*) inBounds: Rect; inKind: ThemeButtonKind; const (*var*) inNewInfo: ThemeButtonDrawInfo; outRegion: RgnHandle ): OSStatus; external name '_GetThemeButtonRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeButtonContentBounds()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeButtonContentBounds( const (*var*) inBounds: Rect; inKind: ThemeButtonKind; const (*var*) inDrawInfo: ThemeButtonDrawInfo; var outBounds: Rect ): OSStatus; external name '_GetThemeButtonContentBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeButtonBackgroundBounds()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeButtonBackgroundBounds( const (*var*) inBounds: Rect; inKind: ThemeButtonKind; const (*var*) inDrawInfo: ThemeButtonDrawInfo; var outBounds: Rect ): OSStatus; external name '_GetThemeButtonBackgroundBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ INTERFACE SOUNDS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  X ALERT: Please note that the sound APIs do not work on Mac OS X at present.                    }
{
 *  PlayThemeSound()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function PlayThemeSound( kind: ThemeSoundKind ): OSStatus; external name '_PlayThemeSound';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  BeginThemeDragSound()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function BeginThemeDragSound( kind: ThemeDragSoundKind ): OSStatus; external name '_BeginThemeDragSound';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  EndThemeDragSound()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function EndThemeDragSound: OSStatus; external name '_EndThemeDragSound';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ PRIMITIVES ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  DrawThemeTickMark()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeTickMark( const (*var*) bounds: Rect; state: ThemeDrawState ): OSStatus; external name '_DrawThemeTickMark';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeChasingArrows()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeChasingArrows( const (*var*) bounds: Rect; index: UInt32; state: ThemeDrawState; eraseProc: ThemeEraseUPP { can be NULL }; eraseData: UInt32 ): OSStatus; external name '_DrawThemeChasingArrows';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemePopupArrow()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemePopupArrow( const (*var*) bounds: Rect; orientation: ThemeArrowOrientation; size: ThemePopupArrowSize; state: ThemeDrawState; eraseProc: ThemeEraseUPP { can be NULL }; eraseData: UInt32 ): OSStatus; external name '_DrawThemePopupArrow';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeStandaloneGrowBox()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeStandaloneGrowBox( origin: Point; growDirection: ThemeGrowDirection; isSmall: Boolean; state: ThemeDrawState ): OSStatus; external name '_DrawThemeStandaloneGrowBox';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DrawThemeStandaloneNoGrowBox()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DrawThemeStandaloneNoGrowBox( origin: Point; growDirection: ThemeGrowDirection; isSmall: Boolean; state: ThemeDrawState ): OSStatus; external name '_DrawThemeStandaloneNoGrowBox';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeStandaloneGrowBoxBounds()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeStandaloneGrowBoxBounds( origin: Point; growDirection: ThemeGrowDirection; isSmall: Boolean; var bounds: Rect ): OSStatus; external name '_GetThemeStandaloneGrowBoxBounds';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ DRAWING STATE ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ The following routines help you save and restore the drawing state in a theme-savvy manner. With }
{ these weapons in your arsenal, there is no grafport you cannot tame. Use ThemeGetDrawingState to }
{ get the current drawing settings for the current port. It will return an opaque object for you   }
{ to pass into SetThemeDrawingState later on. When you are finished with the state, call the       }
{ DisposeThemeDrawingState routine. You can alternatively pass true into the inDisposeNow          }
{ parameter of the SetThemeDrawingState routine.  You can use this routine to copy the drawing     }
{ state from one port to another as well.                                                          }
{ As of this writing (Mac OS 9.1 and Mac OS X), Get/SetThemeDrawingState will save and restore     }
{ this data in the port:                                                                           }
{      pen size                                                                                    }
{      pen location                                                                                }
{      pen mode                                                                                    }
{      pen Pattern and PixPat                                                                      }
{      background Pattern and PixPat                                                               }
{      RGB foreground and background colors                                                        }
{      text mode                                                                                   }
{      pattern origin                                                                              }
{ Get/SetThemeDrawingState may save and restore additional port state in the future, but you can   }
{ rely on them to always save at least this port state.                                            }
{
 *  NormalizeThemeDrawingState()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function NormalizeThemeDrawingState: OSStatus; external name '_NormalizeThemeDrawingState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeDrawingState()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeDrawingState( var outState: ThemeDrawingState ): OSStatus; external name '_GetThemeDrawingState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThemeDrawingState()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function SetThemeDrawingState( inState: ThemeDrawingState; inDisposeNow: Boolean ): OSStatus; external name '_SetThemeDrawingState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DisposeThemeDrawingState()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function DisposeThemeDrawingState( inState: ThemeDrawingState ): OSStatus; external name '_DisposeThemeDrawingState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ MISCELLANEOUS ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ ApplyThemeBackground is used to set up the background for embedded controls  }
{ It is normally called by controls that are embedders. The standard controls  }
{ call this API to ensure a correct background for the current theme. You pass }
{ in the same rectangle you would if you were calling the drawing primitive.   }
{
 *  ApplyThemeBackground()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function ApplyThemeBackground( inKind: ThemeBackgroundKind; const (*var*) bounds: Rect; inState: ThemeDrawState; inDepth: SInt16; inColorDev: Boolean ): OSStatus; external name '_ApplyThemeBackground';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetThemeTextColorForWindow() has moved to MacWindows.h
 }
{
 *  IsValidAppearanceFileType()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function IsValidAppearanceFileType( fileType: OSType ): Boolean; external name '_IsValidAppearanceFileType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeBrushAsColor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeBrushAsColor( inBrush: ThemeBrush; inDepth: SInt16; inColorDev: Boolean; var outColor: RGBColor ): OSStatus; external name '_GetThemeBrushAsColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetThemeTextColor()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in AppearanceLib 1.1 and later
 }
function GetThemeTextColor( inColor: ThemeTextColor; inDepth: SInt16; inColorDev: Boolean; var outColor: RGBColor ): OSStatus; external name '_GetThemeTextColor';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ BEGIN CARBON ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  GetThemeMetric()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function GetThemeMetric( inMetric: ThemeMetric; var outMetric: SInt32 ): OSStatus; external name '_GetThemeMetric';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CopyThemeIdentifier()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Non-Carbon CFM:   not available
 }
function CopyThemeIdentifier( var outIdentifier: CFStringRef ): OSStatus; external name '_CopyThemeIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Obsolete symbolic names                                                                          }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Obsolete error codes - use the new ones, s'il vous plait / kudasai }
const
	appearanceBadBrushIndexErr = themeInvalidBrushErr; { pattern index invalid }
	appearanceProcessRegisteredErr = themeProcessRegisteredErr;
	appearanceProcessNotRegisteredErr = themeProcessNotRegisteredErr;
	appearanceBadTextColorIndexErr = themeBadTextColorErr;
	appearanceThemeHasNoAccents = themeHasNoAccentsErr;
	appearanceBadCursorIndexErr = themeBadCursorIndexErr;

const
	kThemeActiveDialogBackgroundBrush = kThemeBrushDialogBackgroundActive;
	kThemeInactiveDialogBackgroundBrush = kThemeBrushDialogBackgroundInactive;
	kThemeActiveAlertBackgroundBrush = kThemeBrushAlertBackgroundActive;
	kThemeInactiveAlertBackgroundBrush = kThemeBrushAlertBackgroundInactive;
	kThemeActiveModelessDialogBackgroundBrush = kThemeBrushModelessDialogBackgroundActive;
	kThemeInactiveModelessDialogBackgroundBrush = kThemeBrushModelessDialogBackgroundInactive;
	kThemeActiveUtilityWindowBackgroundBrush = kThemeBrushUtilityWindowBackgroundActive;
	kThemeInactiveUtilityWindowBackgroundBrush = kThemeBrushUtilityWindowBackgroundInactive;
	kThemeListViewSortColumnBackgroundBrush = kThemeBrushListViewSortColumnBackground;
	kThemeListViewBackgroundBrush = kThemeBrushListViewBackground;
	kThemeIconLabelBackgroundBrush = kThemeBrushIconLabelBackground;
	kThemeListViewSeparatorBrush = kThemeBrushListViewSeparator;
	kThemeChasingArrowsBrush = kThemeBrushChasingArrows;
	kThemeDragHiliteBrush = kThemeBrushDragHilite;
	kThemeDocumentWindowBackgroundBrush = kThemeBrushDocumentWindowBackground;
	kThemeFinderWindowBackgroundBrush = kThemeBrushFinderWindowBackground;

const
	kThemeActiveScrollBarDelimiterBrush = kThemeBrushScrollBarDelimiterActive;
	kThemeInactiveScrollBarDelimiterBrush = kThemeBrushScrollBarDelimiterInactive;
	kThemeFocusHighlightBrush = kThemeBrushFocusHighlight;
	kThemeActivePopupArrowBrush = kThemeBrushPopupArrowActive;
	kThemePressedPopupArrowBrush = kThemeBrushPopupArrowPressed;
	kThemeInactivePopupArrowBrush = kThemeBrushPopupArrowInactive;
	kThemeAppleGuideCoachmarkBrush = kThemeBrushAppleGuideCoachmark;

const
	kThemeActiveDialogTextColor = kThemeTextColorDialogActive;
	kThemeInactiveDialogTextColor = kThemeTextColorDialogInactive;
	kThemeActiveAlertTextColor = kThemeTextColorAlertActive;
	kThemeInactiveAlertTextColor = kThemeTextColorAlertInactive;
	kThemeActiveModelessDialogTextColor = kThemeTextColorModelessDialogActive;
	kThemeInactiveModelessDialogTextColor = kThemeTextColorModelessDialogInactive;
	kThemeActiveWindowHeaderTextColor = kThemeTextColorWindowHeaderActive;
	kThemeInactiveWindowHeaderTextColor = kThemeTextColorWindowHeaderInactive;
	kThemeActivePlacardTextColor = kThemeTextColorPlacardActive;
	kThemeInactivePlacardTextColor = kThemeTextColorPlacardInactive;
	kThemePressedPlacardTextColor = kThemeTextColorPlacardPressed;
	kThemeActivePushButtonTextColor = kThemeTextColorPushButtonActive;
	kThemeInactivePushButtonTextColor = kThemeTextColorPushButtonInactive;
	kThemePressedPushButtonTextColor = kThemeTextColorPushButtonPressed;
	kThemeActiveBevelButtonTextColor = kThemeTextColorBevelButtonActive;
	kThemeInactiveBevelButtonTextColor = kThemeTextColorBevelButtonInactive;
	kThemePressedBevelButtonTextColor = kThemeTextColorBevelButtonPressed;
	kThemeActivePopupButtonTextColor = kThemeTextColorPopupButtonActive;
	kThemeInactivePopupButtonTextColor = kThemeTextColorPopupButtonInactive;
	kThemePressedPopupButtonTextColor = kThemeTextColorPopupButtonPressed;
	kThemeIconLabelTextColor = kThemeTextColorIconLabel;
	kThemeListViewTextColor = kThemeTextColorListView;

const
	kThemeActiveDocumentWindowTitleTextColor = kThemeTextColorDocumentWindowTitleActive;
	kThemeInactiveDocumentWindowTitleTextColor = kThemeTextColorDocumentWindowTitleInactive;
	kThemeActiveMovableModalWindowTitleTextColor = kThemeTextColorMovableModalWindowTitleActive;
	kThemeInactiveMovableModalWindowTitleTextColor = kThemeTextColorMovableModalWindowTitleInactive;
	kThemeActiveUtilityWindowTitleTextColor = kThemeTextColorUtilityWindowTitleActive;
	kThemeInactiveUtilityWindowTitleTextColor = kThemeTextColorUtilityWindowTitleInactive;
	kThemeActivePopupWindowTitleColor = kThemeTextColorPopupWindowTitleActive;
	kThemeInactivePopupWindowTitleColor = kThemeTextColorPopupWindowTitleInactive;
	kThemeActiveRootMenuTextColor = kThemeTextColorRootMenuActive;
	kThemeSelectedRootMenuTextColor = kThemeTextColorRootMenuSelected;
	kThemeDisabledRootMenuTextColor = kThemeTextColorRootMenuDisabled;
	kThemeActiveMenuItemTextColor = kThemeTextColorMenuItemActive;
	kThemeSelectedMenuItemTextColor = kThemeTextColorMenuItemSelected;
	kThemeDisabledMenuItemTextColor = kThemeTextColorMenuItemDisabled;
	kThemeActivePopupLabelTextColor = kThemeTextColorPopupLabelActive;
	kThemeInactivePopupLabelTextColor = kThemeTextColorPopupLabelInactive;

const
	kAEThemeSwitch = kAEAppearanceChanged; { Event ID's: Theme Switched }

const
	kThemeNoAdornment = kThemeAdornmentNone;
	kThemeDefaultAdornment = kThemeAdornmentDefault;
	kThemeFocusAdornment = kThemeAdornmentFocus;
	kThemeRightToLeftAdornment = kThemeAdornmentRightToLeft;
	kThemeDrawIndicatorOnly = kThemeAdornmentDrawIndicatorOnly;

const
	kThemeBrushPassiveAreaFill = kThemeBrushStaticAreaFill;

const
	kThemeMetricCheckBoxGlyphHeight = kThemeMetricCheckBoxHeight;
	kThemeMetricRadioButtonGlyphHeight = kThemeMetricRadioButtonHeight;
	kThemeMetricDisclosureButtonSize = kThemeMetricDisclosureButtonHeight;
	kThemeMetricBestListHeaderHeight = kThemeMetricListHeaderHeight;
	kThemeMetricSmallProgressBarThickness = kThemeMetricNormalProgressBarThickness; { obsolete }
	kThemeMetricProgressBarThickness = kThemeMetricLargeProgressBarThickness; { obsolete }

const
	kThemeScrollBar = kThemeMediumScrollBar;
	kThemeSlider = kThemeMediumSlider;
	kThemeProgressBar = kThemeMediumProgressBar;
	kThemeIndeterminateBar = kThemeMediumIndeterminateBar;




end.

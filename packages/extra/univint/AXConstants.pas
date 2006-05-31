{
 *  AXConstants.h
 *
 *  Created by John Louch on Tue Mar 26 2002.
 *  Copyright (c) 2002 Apple Computer, Inc. All rights reserved.
 *
 }
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit AXConstants;
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
uses MacTypes;
{$ALIGN MAC68K}
// standard attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRoleAttribute CFSTRP('AXRole')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRoleDescriptionAttribute CFSTRP('AXRoleDescription')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSubroleAttribute CFSTRP('AXSubrole')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHelpAttribute CFSTRP('AXHelp')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTitleAttribute CFSTRP('AXTitle')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXValueAttribute CFSTRP('AXValue')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinValueAttribute CFSTRP('AXMinValue')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMaxValueAttribute CFSTRP('AXMaxValue')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXEnabledAttribute CFSTRP('AXEnabled')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedAttribute CFSTRP('AXFocused')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXParentAttribute CFSTRP('AXParent')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXChildrenAttribute CFSTRP('AXChildren')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedChildrenAttribute CFSTRP('AXSelectedChildren')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVisibleChildrenAttribute CFSTRP('AXVisibleChildren')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowAttribute CFSTRP('AXWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPositionAttribute CFSTRP('AXPosition')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSizeAttribute CFSTRP('AXSize')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXOrientationAttribute CFSTRP('AXOrientation')}
{$endc}

// text-specific attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTextAttribute CFSTRP('AXText')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedTextAttribute CFSTRP('AXSelectedText')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedTextRangeAttribute CFSTRP('AXSelectedTextRange')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVisibleTextAttribute CFSTRP('AXVisibleText')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVisibleCharacterRangeAttribute CFSTRP('AXVisibleCharacterRange')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXNumberOfCharactersAttribute CFSTRP('AXNumberOfCharacters')}
{$endc}

// window-specific attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMainAttribute CFSTRP('AXMain')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinimizedAttribute CFSTRP('AXMinimized')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCloseButtonAttribute CFSTRP('AXCloseButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXZoomButtonAttribute CFSTRP('AXZoomButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinimizeButtonAttribute CFSTRP('AXMinimizeButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXToolbarButtonAttribute CFSTRP('AXToolbarButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXProxyAttribute CFSTRP('AXProxy')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXGrowAreaAttribute CFSTRP('AXGrowArea')}
{$endc}

// new window-specific attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXModalAttribute CFSTRP('AXModal')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDefaultButtonAttribute CFSTRP('AXDefaultButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCancelButtonAttribute CFSTRP('AXCancelButton')}
{$endc}

// menu-specific attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemCmdCharAttribute CFSTRP('AXMenuItemCmdChar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemCmdVirtualKeyAttribute CFSTRP('AXMenuItemCmdVirtualKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemCmdGlyphAttribute CFSTRP('AXMenuItemCmdGlyph')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemCmdModifiersAttribute CFSTRP('AXMenuItemCmdModifiers')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemMarkCharAttribute CFSTRP('AXMenuItemMarkChar')}
{$endc}

// application-specific attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuBarAttribute CFSTRP('AXMenuBar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowsAttribute CFSTRP('AXWindows')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFrontmostAttribute CFSTRP('AXFrontmost')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHiddenAttribute CFSTRP('AXHidden')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMainWindowAttribute CFSTRP('AXMainWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedWindowAttribute CFSTRP('AXFocusedWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedUIElementAttribute CFSTRP('AXFocusedUIElement')}
{$endc} 

// misc attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHeaderAttribute CFSTRP('AXHeader')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXEditedAttribute CFSTRP('AXEdited')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXValueIncrementAttribute CFSTRP('AXValueIncrement')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXValueWrapsAttribute CFSTRP('AXValueWraps')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTabsAttribute CFSTRP('AXTabs')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTitleUIElementAttribute CFSTRP('AXTitleUIElement')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHorizontalScrollBarAttribute CFSTRP('AXHorizontalScrollBar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVerticalScrollBarAttribute CFSTRP('AXVerticalScrollBar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXOverflowButtonAttribute CFSTRP('AXOverflowButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFilenameAttribute CFSTRP('AXFilename')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXExpandedAttribute CFSTRP('AXExpanded')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedAttribute CFSTRP('AXSelected')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSplittersAttribute CFSTRP('AXSplitters')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXNextContentsAttribute CFSTRP('AXNextContents')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDocumentAttribute CFSTRP('AXDocument')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDecrementButtonAttribute CFSTRP('AXDecrementButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementButtonAttribute CFSTRP('AXIncrementButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPreviousContentsAttribute CFSTRP('AXPreviousContents')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXContentsAttribute CFSTRP('AXContents')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementorAttribute CFSTRP('AXIncrementor')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHourFieldAttribute CFSTRP('AXHourField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinuteFieldAttribute CFSTRP('AXMinuteField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSecondFieldAttribute CFSTRP('AXSecondField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXAMPMFieldAttribute CFSTRP('AXAMPMField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDayFieldAttribute CFSTRP('AXDayField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMonthFieldAttribute CFSTRP('AXMonthField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXYearFieldAttribute CFSTRP('AXYearField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXColumnTitlesAttribute CFSTRP('AXColumnTitles')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXURLAttribute CFSTRP('AXURL')}
{$endc}

// table/outline view attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRowsAttribute CFSTRP('AXRows')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVisibleRowsAttribute CFSTRP('AXVisibleRows')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedRowsAttribute CFSTRP('AXSelectedRows')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXColumnsAttribute CFSTRP('AXColumns')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVisibleColumnsAttribute CFSTRP('AXVisibleColumns')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSelectedColumnsAttribute CFSTRP('AXSelectedColumns')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSortDirectionAttribute CFSTRP('AXSortDirection')}
{$endc}

// outline attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDisclosingAttribute CFSTRP('AXDisclosing')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDisclosedRowsAttribute CFSTRP('AXDisclosedRows')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDisclosedByRowAttribute CFSTRP('AXDisclosedByRow')}
{$endc}

// system-wide attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFocusedApplicationAttribute CFSTRP('AXFocusedApplication')}
{$endc}

//
// Parameterized Attributes
//

// Text Suite Parameterized Attributes
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXLineForIndexParameterizedAttribute CFSTRP('AXLineForIndex')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRangeForLineParameterizedAttribute CFSTRP('AXRangeForLine')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXStringForRangeParameterizedAttribute CFSTRP('AXStringForRange')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRangeForPositionParameterizedAttribute CFSTRP('AXRangeForPosition')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRangeForIndexParameterizedAttribute CFSTRP('AXRangeForIndex')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXBoundsForRangeParameterizedAttribute CFSTRP('AXBoundsForRange')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRTFForRangeParameterizedAttribute CFSTRP('AXRTFForRange')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXStyleRangeForIndexParameterizedAttribute CFSTRP('AXStyleRangeForIndex')}
{$endc}

//
// standard values
//

// orientations (see kAXOrientationAttribute)
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHorizontalOrientationValue CFSTRP('AXHorizontalOrientation')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXVerticalOrientationValue CFSTRP('AXVerticalOrientation')}
{$endc}

// standard actions
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPressAction CFSTRP('AXPress')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementAction CFSTRP('AXIncrement')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDecrementAction CFSTRP('AXDecrement')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXConfirmAction CFSTRP('AXConfirm')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPickAction CFSTRP('AXPick')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCancelAction CFSTRP('AXCancel')}
{$endc}

// new actions
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRaiseAction CFSTRP('AXRaise')}
{$endc}

// standard roles
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationRole CFSTRP('AXApplication')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSystemWideRole CFSTRP('AXSystemWide')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXWindowRole CFSTRP('AXWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSheetRole CFSTRP('AXSheet')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDrawerRole CFSTRP('AXDrawer')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXGrowAreaRole CFSTRP('AXGrowArea')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXImageRole CFSTRP('AXImage')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXUnknownRole CFSTRP('AXUnknown')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXButtonRole CFSTRP('AXButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRadioButtonRole CFSTRP('AXRadioButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCheckBoxRole CFSTRP('AXCheckBox')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPopUpButtonRole CFSTRP('AXPopUpButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuButtonRole CFSTRP('AXMenuButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTabGroupRole CFSTRP('AXTabGroup')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTableRole CFSTRP('AXTable')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXColumnRole CFSTRP('AXColumn')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRowRole CFSTRP('AXRow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXOutlineRole CFSTRP('AXOutline')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXBrowserRole CFSTRP('AXBrowser')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXScrollAreaRole CFSTRP('AXScrollArea')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXScrollBarRole CFSTRP('AXScrollBar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRadioGroupRole CFSTRP('AXRadioGroup')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXListRole CFSTRP('AXList')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXGroupRole CFSTRP('AXGroup')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXValueIndicatorRole CFSTRP('AXValueIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXComboBoxRole CFSTRP('AXComboBox')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSliderRole CFSTRP('AXSlider')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementorRole CFSTRP('AXIncrementor')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXBusyIndicatorRole CFSTRP('AXBusyIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXProgressIndicatorRole CFSTRP('AXProgressIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXToolbarRole CFSTRP('AXToolbar')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTextFieldRole CFSTRP('AXTextField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTextAreaRole CFSTRP('AXTextArea')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXStaticTextRole CFSTRP('AXStaticText')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuBarRole CFSTRP('AXMenuBar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuBarItemRole CFSTRP('AXMenuBarItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuRole CFSTRP('AXMenu')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMenuItemRole CFSTRP('AXMenuItem')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSplitGroupRole CFSTRP('AXSplitGroup')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSplitterRole CFSTRP('AXSplitter')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXColorWellRole CFSTRP('AXColorWell')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTimeFieldRole CFSTRP('AXTimeField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDateFieldRole CFSTRP('AXDateField')}
{$endc}

// standard subroles
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCloseButtonSubrole CFSTRP('AXCloseButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinimizeButtonSubrole CFSTRP('AXMinimizeButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXZoomButtonSubrole CFSTRP('AXZoomButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXToolbarButtonSubrole CFSTRP('AXToolbarButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSecureTextFieldSubrole CFSTRP('AXSecureTextField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTableRowSubrole CFSTRP('AXTableRow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXOutlineRowSubrole CFSTRP('AXOutlineRow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXUnknownSubrole CFSTRP('AXUnknown')}
{$endc}

// new subroles
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXStandardWindowSubrole CFSTRP('AXStandardWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDialogSubrole CFSTRP('AXDialog')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSystemDialogSubrole CFSTRP('AXSystemDialog')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFloatingWindowSubrole CFSTRP('AXFloatingWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSystemFloatingWindowSubrole CFSTRP('AXSystemFloatingWindow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementArrowSubrole CFSTRP('AXIncrementArrow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDecrementArrowSubrole CFSTRP('AXDecrementArrow')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIncrementPageSubrole CFSTRP('AXIncrementPage')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDecrementPageSubrole CFSTRP('AXDecrementPage')}
{$endc}

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

// new drawer & sheet notifications
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDrawerCreatedNotification CFSTRP('AXDrawerCreated')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSheetCreatedNotification CFSTRP('AXSheetCreated')}
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


// obsolete constants will be removed soon
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRelevanceIndicatorRole CFSTRP('AXRelevanceIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXIsEditableAttribute CFSTRP('AXIsEditable')}
{$endc}

end.

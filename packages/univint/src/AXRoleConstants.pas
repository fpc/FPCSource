{
 *  AXRoleConstants.h
 *  HIServices
 *
 *  Created by John Louch on Wed Feb 25 2004.
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }

{  Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }

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

unit AXRoleConstants;
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
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
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Roles                                                                                }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{
	Every role offers a variety of attributes. There are some attributes that must be
	supported by every element, regardless of role. Other attributes will be supported
	by an element if/when appropriate data is supplied by the application. These
	attributes' meanings and values are generally obvious. In order to save space, the
	following attributes are not listed in the role documentation unless the role
	handles them in a special fashion:
	
		AXRole
		AXRoleDescription
		AXDescription
		AXHelp
		AXParent
		AXChildren
		AXWindow
		AXTopLevelUIElement
		AXEnabled
		AXSize
		AXPosition
	
	Every attribute supported by a given role may have one or more symbols after its
	name:
		w means the attribute is writable.
		o means it is an optional attribute that doesn't necessarily need to be
			supported by all elements with that role.

	TBD:
		Add a general section answering the following questions:
			When and why would a developer create a new role?
			When and why would a developer create a new subrole?
		Add a Quick Reference section, like the one at the top of the attributes.
}

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


{
	kAXOutlineRole
	
	An element that contains row-based data. It may use disclosure triangles to manage the
	display of hierarchies within the data. It may arrange each row's data into columns and
	offer a header button above each column. The best example is the list view in a Finder
	window or Open/Save dialog.

	Outlines are typically children of AXScrollAreas, which manages the horizontal and/or
	vertical scrolling for the outline. Outlines are expected to follow certain conventions
	with respect to their hierarchy of sub-elements. In particular, if the outline uses
	columns, the data should be accessible via either rows or columns. Thus, the data in a
	given cell will be represented as two diffrent elements. Here's a hierarchy for a
	typical outline:
	
		AXScrollArea (parent of the outline)
			AXScrollBar (if necessary, horizontal)
			AXScrollBar (if necessary, vertical)
			AXOutline
				AXGroup (header buttons, optional)
					AXButton, AXMenuButton, or <Varies> (header button)
					...
				AXRow (first row)
					AXStaticText (just one possible example)
					AXButton (just another possible example)
					AXTextField (ditto)
					AXCheckBox (ditto)
				AXRow (as above)
				...
				AXColumn (first column)
					AXStaticText (assumes the first column displays text)
					AXStaticText
					...
				AXColumn (second column)
					AXButton (assumes the second column displays buttons)
					AXButton
					...
				...
				
    Attributes:
        AXFocused (w)
        AXRows - Array of subset of AXChildren that are rows.
        AXVisibleRows - Array of subset of AXRows that are visible.
        AXSelectedRows (w) Array of subset of AXRows that are selected.
        AXColumns - Array of subset of children that are columns.
        AXVisibleColumns - Array of subset of columns that are visible.
        AXSelectedColumns (o) Array of subset of columns that are selected.
        AXHeader (o) The AXGroup element that contains the header buttons.
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXOutlineRole CFSTRP('AXOutline')}
{$endc}


{
	kAXBrowserRole
	
	An element that contains columns of hierarchical data. Examples include the column view
	in Finder windows and Open/Save dialogs. Carbon's Data Browser in column view mode
	represents itself as an AXBrowser. Cocoa's NSBrowser represents itself as an AXBrowser.
	
	Browser elements are expected to have a particular hierarchy of sub-elements within it.
	In particular, the child of an AXBrowser must be an AXScrollArea that manages the
	horizontal scrolling. The horizontal AXScrollArea must include a child for each column
	the interface displays. Columns can be any role that makes sense. Typically, columns
	are vertical AXScrollAreas with AXList children. Here's a hierarchy for a typical
	browser:
	
		AXBrowser
			AXScrollArea (manages the horizontal scrolling)
				AXScrollBar (horizontal scroll bar)
				AXScrollArea (first column)
					AXScrollBar (column's vertical scroll bar)
					AXList (column content is typically a list, but it could be another role)
						<Varies> (cell)
						...
						<Varies> (cell)
				AXScrollArea (second column)
					...
				AXScrollArea (third column)
					...
				AXGroup (preview column)
					...
	
	Attributes:
		AXFocused (w)
		AXColumns - Array of the grandchild column elements, which are typically
			of the AXScrollArea role.
		AXVisibleColumns - Array of the subset of elements in the AXColumns array
			that are currently visible.
		AXColumnTitles (o)
		AXHorizontalScrollBar - The horizontal AXScrollBar of the browser's child
			AXScrollArea.
}
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
{$definec kAXRelevanceIndicatorRole CFSTRP('AXRelevanceIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXToolbarRole CFSTRP('AXToolbar')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDisclosureTriangleRole CFSTRP('AXDisclosureTriangle')}
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

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHelpTagRole CFSTRP('AXHelpTag')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMatteRole CFSTRP('AXMatteRole')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDockItemRole CFSTRP('AXDockItem')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRulerRole CFSTRP('AXRuler')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRulerMarkerRole CFSTRP('AXRulerMarker')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXGridRole CFSTRP('AXGrid')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXLevelIndicatorRole CFSTRP('AXLevelIndicator')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXCellRole CFSTRP('AXCell')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXLayoutAreaRole CFSTRP('AXLayoutArea')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXLayoutItemRole CFSTRP('AXLayoutItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXHandleRole CFSTRP('AXHandle')}
{$endc}

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXPopoverRole CFSTRP('AXPopover')}
{$endc}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Subroles                                                                                }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

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
{$definec kAXFullScreenButtonSubrole CFSTRP('AXFullScreenButton')}
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
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSortButtonSubrole CFSTRP('AXSortButton')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSearchFieldSubrole CFSTRP('AXSearchField')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTimelineSubrole CFSTRP('AXTimeline')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXRatingIndicatorSubrole CFSTRP('AXRatingIndicator')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXContentListSubrole CFSTRP('AXContentList')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDefinitionListSubrole CFSTRP('AXDefinitionList')}
{$endc}

// dock subroles
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXApplicationDockItemSubrole CFSTRP('AXApplicationDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDocumentDockItemSubrole CFSTRP('AXDocumentDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXFolderDockItemSubrole CFSTRP('AXFolderDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXMinimizedWindowDockItemSubrole CFSTRP('AXMinimizedWindowDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXURLDockItemSubrole CFSTRP('AXURLDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXDockExtraDockItemSubrole CFSTRP('AXDockExtraDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXTrashDockItemSubrole CFSTRP('AXTrashDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXSeparatorDockItemSubrole CFSTRP('AXSeparatorDockItem')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kAXProcessSwitcherListSubrole CFSTRP('AXProcessSwitcherList')}
{$endc}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}

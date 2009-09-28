{ Parsed from Appkit.framework NSAccessibility.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSACCESSIBILITY_PAS_T}
{$define NSACCESSIBILITY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSACCESSIBILITY_PAS_R}
{$define NSACCESSIBILITY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSACCESSIBILITY_PAS_F}
{$define NSACCESSIBILITY_PAS_F}

{ Functions }
function NSAccessibilityRoleDescription(rolePointer: NSString; subrolePointer: NSString): NSString; cdecl; external name 'NSAccessibilityRoleDescription';
function NSAccessibilityRoleDescriptionForUIElement(element: id): NSString; cdecl; external name 'NSAccessibilityRoleDescriptionForUIElement';
function NSAccessibilityActionDescription(actionPointer: NSString): NSString; cdecl; external name 'NSAccessibilityActionDescription';
procedure NSAccessibilityRaiseBadArgumentException(element: id; attributePointer: NSString; value: id); cdecl; external name 'NSAccessibilityRaiseBadArgumentException';
function NSAccessibilityUnignoredAncestor(element: id): id; cdecl; external name 'NSAccessibilityUnignoredAncestor';
function NSAccessibilityUnignoredDescendant(element: id): id; cdecl; external name 'NSAccessibilityUnignoredDescendant';
function NSAccessibilityUnignoredChildren(originalChildrenPointer: NSArray): NSArray; cdecl; external name 'NSAccessibilityUnignoredChildren';
function NSAccessibilityUnignoredChildrenForOnlyChild(originalChild: id): NSArray; cdecl; external name 'NSAccessibilityUnignoredChildrenForOnlyChild';
procedure NSAccessibilityPostNotification(element: id; notificationPointer: NSString); cdecl; external name 'NSAccessibilityPostNotification';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSACCESSIBILITY_PAS_S}
{$define NSACCESSIBILITY_PAS_S}

{ External string constants }
var
  NSAccessibilityErrorCodeExceptionInfo: NSString; external name '_NSAccessibilityErrorCodeExceptionInfo';
  NSAccessibilityRoleAttribute: NSString; external name '_NSAccessibilityRoleAttribute';
  NSAccessibilityRoleDescriptionAttribute: NSString; external name '_NSAccessibilityRoleDescriptionAttribute';
  NSAccessibilitySubroleAttribute: NSString; external name '_NSAccessibilitySubroleAttribute';
  NSAccessibilityHelpAttribute: NSString; external name '_NSAccessibilityHelpAttribute';
  NSAccessibilityValueAttribute: NSString; external name '_NSAccessibilityValueAttribute';
  NSAccessibilityMinValueAttribute: NSString; external name '_NSAccessibilityMinValueAttribute';
  NSAccessibilityMaxValueAttribute: NSString; external name '_NSAccessibilityMaxValueAttribute';
  NSAccessibilityEnabledAttribute: NSString; external name '_NSAccessibilityEnabledAttribute';
  NSAccessibilityFocusedAttribute: NSString; external name '_NSAccessibilityFocusedAttribute';
  NSAccessibilityParentAttribute: NSString; external name '_NSAccessibilityParentAttribute';
  NSAccessibilityChildrenAttribute: NSString; external name '_NSAccessibilityChildrenAttribute';
  NSAccessibilityWindowAttribute: NSString; external name '_NSAccessibilityWindowAttribute';
  NSAccessibilitySelectedChildrenAttribute: NSString; external name '_NSAccessibilitySelectedChildrenAttribute';
  NSAccessibilityVisibleChildrenAttribute: NSString; external name '_NSAccessibilityVisibleChildrenAttribute';
  NSAccessibilityPositionAttribute: NSString; external name '_NSAccessibilityPositionAttribute';
  NSAccessibilitySizeAttribute: NSString; external name '_NSAccessibilitySizeAttribute';
  NSAccessibilityContentsAttribute: NSString; external name '_NSAccessibilityContentsAttribute';
  NSAccessibilityTitleAttribute: NSString; external name '_NSAccessibilityTitleAttribute';
  NSAccessibilityPreviousContentsAttribute: NSString; external name '_NSAccessibilityPreviousContentsAttribute';
  NSAccessibilityNextContentsAttribute: NSString; external name '_NSAccessibilityNextContentsAttribute';
  NSAccessibilityHeaderAttribute: NSString; external name '_NSAccessibilityHeaderAttribute';
  NSAccessibilityEditedAttribute: NSString; external name '_NSAccessibilityEditedAttribute';
  NSAccessibilityTabsAttribute: NSString; external name '_NSAccessibilityTabsAttribute';
  NSAccessibilityHorizontalScrollBarAttribute: NSString; external name '_NSAccessibilityHorizontalScrollBarAttribute';
  NSAccessibilityVerticalScrollBarAttribute: NSString; external name '_NSAccessibilityVerticalScrollBarAttribute';
  NSAccessibilityOverflowButtonAttribute: NSString; external name '_NSAccessibilityOverflowButtonAttribute';
  NSAccessibilityIncrementButtonAttribute: NSString; external name '_NSAccessibilityIncrementButtonAttribute';
  NSAccessibilityDecrementButtonAttribute: NSString; external name '_NSAccessibilityDecrementButtonAttribute';
  NSAccessibilityFilenameAttribute: NSString; external name '_NSAccessibilityFilenameAttribute';
  NSAccessibilityExpandedAttribute: NSString; external name '_NSAccessibilityExpandedAttribute';
  NSAccessibilitySelectedAttribute: NSString; external name '_NSAccessibilitySelectedAttribute';
  NSAccessibilitySplittersAttribute: NSString; external name '_NSAccessibilitySplittersAttribute';
  NSAccessibilityDocumentAttribute: NSString; external name '_NSAccessibilityDocumentAttribute';
  NSAccessibilityTitleUIElementAttribute: NSString; external name '_NSAccessibilityTitleUIElementAttribute';
  NSAccessibilitySelectedTextAttribute: NSString; external name '_NSAccessibilitySelectedTextAttribute';
  NSAccessibilitySelectedTextRangeAttribute: NSString; external name '_NSAccessibilitySelectedTextRangeAttribute';
  NSAccessibilityMainAttribute: NSString; external name '_NSAccessibilityMainAttribute';
  NSAccessibilityMinimizedAttribute: NSString; external name '_NSAccessibilityMinimizedAttribute';
  NSAccessibilityCloseButtonAttribute: NSString; external name '_NSAccessibilityCloseButtonAttribute';
  NSAccessibilityZoomButtonAttribute: NSString; external name '_NSAccessibilityZoomButtonAttribute';
  NSAccessibilityMinimizeButtonAttribute: NSString; external name '_NSAccessibilityMinimizeButtonAttribute';
  NSAccessibilityToolbarButtonAttribute: NSString; external name '_NSAccessibilityToolbarButtonAttribute';
  NSAccessibilityProxyAttribute: NSString; external name '_NSAccessibilityProxyAttribute';
  NSAccessibilityGrowAreaAttribute: NSString; external name '_NSAccessibilityGrowAreaAttribute';
  NSAccessibilityMenuBarAttribute: NSString; external name '_NSAccessibilityMenuBarAttribute';
  NSAccessibilityWindowsAttribute: NSString; external name '_NSAccessibilityWindowsAttribute';
  NSAccessibilityFrontmostAttribute: NSString; external name '_NSAccessibilityFrontmostAttribute';
  NSAccessibilityHiddenAttribute: NSString; external name '_NSAccessibilityHiddenAttribute';
  NSAccessibilityMainWindowAttribute: NSString; external name '_NSAccessibilityMainWindowAttribute';
  NSAccessibilityFocusedWindowAttribute: NSString; external name '_NSAccessibilityFocusedWindowAttribute';
  NSAccessibilityFocusedUIElementAttribute: NSString; external name '_NSAccessibilityFocusedUIElementAttribute';
  NSAccessibilityOrientationAttribute: NSString; external name '_NSAccessibilityOrientationAttribute';
  NSAccessibilityVerticalOrientationValue: NSString; external name '_NSAccessibilityVerticalOrientationValue';
  NSAccessibilityHorizontalOrientationValue: NSString; external name '_NSAccessibilityHorizontalOrientationValue';
  NSAccessibilityColumnTitlesAttribute: NSString; external name '_NSAccessibilityColumnTitlesAttribute';
  NSAccessibilityRowsAttribute: NSString; external name '_NSAccessibilityRowsAttribute';
  NSAccessibilityVisibleRowsAttribute: NSString; external name '_NSAccessibilityVisibleRowsAttribute';
  NSAccessibilitySelectedRowsAttribute: NSString; external name '_NSAccessibilitySelectedRowsAttribute';
  NSAccessibilityColumnsAttribute: NSString; external name '_NSAccessibilityColumnsAttribute';
  NSAccessibilityVisibleColumnsAttribute: NSString; external name '_NSAccessibilityVisibleColumnsAttribute';
  NSAccessibilitySelectedColumnsAttribute: NSString; external name '_NSAccessibilitySelectedColumnsAttribute';
  NSAccessibilityAscendingSortDirectionValue: NSString; external name '_NSAccessibilityAscendingSortDirectionValue';
  NSAccessibilityDescendingSortDirectionValue: NSString; external name '_NSAccessibilityDescendingSortDirectionValue';
  NSAccessibilityUnknownSortDirectionValue: NSString; external name '_NSAccessibilityUnknownSortDirectionValue';
  NSAccessibilityDisclosingAttribute: NSString; external name '_NSAccessibilityDisclosingAttribute';
  NSAccessibilityDisclosedRowsAttribute: NSString; external name '_NSAccessibilityDisclosedRowsAttribute';
  NSAccessibilityDisclosedByRowAttribute: NSString; external name '_NSAccessibilityDisclosedByRowAttribute';
  NSAccessibilityLeftTabStopMarkerTypeValue: NSString; external name '_NSAccessibilityLeftTabStopMarkerTypeValue';
  NSAccessibilityRightTabStopMarkerTypeValue: NSString; external name '_NSAccessibilityRightTabStopMarkerTypeValue';
  NSAccessibilityCenterTabStopMarkerTypeValue: NSString; external name '_NSAccessibilityCenterTabStopMarkerTypeValue';
  NSAccessibilityDecimalTabStopMarkerTypeValue: NSString; external name '_NSAccessibilityDecimalTabStopMarkerTypeValue';
  NSAccessibilityHeadIndentMarkerTypeValue: NSString; external name '_NSAccessibilityHeadIndentMarkerTypeValue';
  NSAccessibilityTailIndentMarkerTypeValue: NSString; external name '_NSAccessibilityTailIndentMarkerTypeValue';
  NSAccessibilityFirstLineIndentMarkerTypeValue: NSString; external name '_NSAccessibilityFirstLineIndentMarkerTypeValue';
  NSAccessibilityUnknownMarkerTypeValue: NSString; external name '_NSAccessibilityUnknownMarkerTypeValue';
  NSAccessibilityInchesUnitValue: NSString; external name '_NSAccessibilityInchesUnitValue';
  NSAccessibilityCentimetersUnitValue: NSString; external name '_NSAccessibilityCentimetersUnitValue';
  NSAccessibilityPointsUnitValue: NSString; external name '_NSAccessibilityPointsUnitValue';
  NSAccessibilityPicasUnitValue: NSString; external name '_NSAccessibilityPicasUnitValue';
  NSAccessibilityUnknownUnitValue: NSString; external name '_NSAccessibilityUnknownUnitValue';
  NSAccessibilityPressAction: NSString; external name '_NSAccessibilityPressAction';
  NSAccessibilityIncrementAction: NSString; external name '_NSAccessibilityIncrementAction';
  NSAccessibilityDecrementAction: NSString; external name '_NSAccessibilityDecrementAction';
  NSAccessibilityConfirmAction: NSString; external name '_NSAccessibilityConfirmAction';
  NSAccessibilityPickAction: NSString; external name '_NSAccessibilityPickAction';
  NSAccessibilityCancelAction: NSString; external name '_NSAccessibilityCancelAction';
  NSAccessibilityRaiseAction: NSString; external name '_NSAccessibilityRaiseAction';
  NSAccessibilityShowMenuAction: NSString; external name '_NSAccessibilityShowMenuAction';
  NSAccessibilityDeleteAction: NSString; external name '_NSAccessibilityDeleteAction';
  NSAccessibilityMainWindowChangedNotification: NSString; external name '_NSAccessibilityMainWindowChangedNotification';
  NSAccessibilityFocusedWindowChangedNotification: NSString; external name '_NSAccessibilityFocusedWindowChangedNotification';
  NSAccessibilityFocusedUIElementChangedNotification: NSString; external name '_NSAccessibilityFocusedUIElementChangedNotification';
  NSAccessibilityApplicationActivatedNotification: NSString; external name '_NSAccessibilityApplicationActivatedNotification';
  NSAccessibilityApplicationDeactivatedNotification: NSString; external name '_NSAccessibilityApplicationDeactivatedNotification';
  NSAccessibilityApplicationHiddenNotification: NSString; external name '_NSAccessibilityApplicationHiddenNotification';
  NSAccessibilityApplicationShownNotification: NSString; external name '_NSAccessibilityApplicationShownNotification';
  NSAccessibilityWindowCreatedNotification: NSString; external name '_NSAccessibilityWindowCreatedNotification';
  NSAccessibilityWindowMovedNotification: NSString; external name '_NSAccessibilityWindowMovedNotification';
  NSAccessibilityWindowResizedNotification: NSString; external name '_NSAccessibilityWindowResizedNotification';
  NSAccessibilityWindowMiniaturizedNotification: NSString; external name '_NSAccessibilityWindowMiniaturizedNotification';
  NSAccessibilityWindowDeminiaturizedNotification: NSString; external name '_NSAccessibilityWindowDeminiaturizedNotification';
  NSAccessibilityDrawerCreatedNotification: NSString; external name '_NSAccessibilityDrawerCreatedNotification';
  NSAccessibilitySheetCreatedNotification: NSString; external name '_NSAccessibilitySheetCreatedNotification';
  NSAccessibilityUIElementDestroyedNotification: NSString; external name '_NSAccessibilityUIElementDestroyedNotification';
  NSAccessibilityValueChangedNotification: NSString; external name '_NSAccessibilityValueChangedNotification';
  NSAccessibilityTitleChangedNotification: NSString; external name '_NSAccessibilityTitleChangedNotification';
  NSAccessibilityResizedNotification: NSString; external name '_NSAccessibilityResizedNotification';
  NSAccessibilityMovedNotification: NSString; external name '_NSAccessibilityMovedNotification';
  NSAccessibilityCreatedNotification: NSString; external name '_NSAccessibilityCreatedNotification';
  NSAccessibilityHelpTagCreatedNotification: NSString; external name '_NSAccessibilityHelpTagCreatedNotification';
  NSAccessibilitySelectedTextChangedNotification: NSString; external name '_NSAccessibilitySelectedTextChangedNotification';
  NSAccessibilityRowCountChangedNotification: NSString; external name '_NSAccessibilityRowCountChangedNotification';
  NSAccessibilitySelectedChildrenChangedNotification: NSString; external name '_NSAccessibilitySelectedChildrenChangedNotification';
  NSAccessibilitySelectedRowsChangedNotification: NSString; external name '_NSAccessibilitySelectedRowsChangedNotification';
  NSAccessibilitySelectedColumnsChangedNotification: NSString; external name '_NSAccessibilitySelectedColumnsChangedNotification';
  NSAccessibilityUnknownRole: NSString; external name '_NSAccessibilityUnknownRole';
  NSAccessibilityButtonRole: NSString; external name '_NSAccessibilityButtonRole';
  NSAccessibilityRadioButtonRole: NSString; external name '_NSAccessibilityRadioButtonRole';
  NSAccessibilityCheckBoxRole: NSString; external name '_NSAccessibilityCheckBoxRole';
  NSAccessibilitySliderRole: NSString; external name '_NSAccessibilitySliderRole';
  NSAccessibilityTabGroupRole: NSString; external name '_NSAccessibilityTabGroupRole';
  NSAccessibilityTextFieldRole: NSString; external name '_NSAccessibilityTextFieldRole';
  NSAccessibilityStaticTextRole: NSString; external name '_NSAccessibilityStaticTextRole';
  NSAccessibilityTextAreaRole: NSString; external name '_NSAccessibilityTextAreaRole';
  NSAccessibilityScrollAreaRole: NSString; external name '_NSAccessibilityScrollAreaRole';
  NSAccessibilityPopUpButtonRole: NSString; external name '_NSAccessibilityPopUpButtonRole';
  NSAccessibilityMenuButtonRole: NSString; external name '_NSAccessibilityMenuButtonRole';
  NSAccessibilityTableRole: NSString; external name '_NSAccessibilityTableRole';
  NSAccessibilityApplicationRole: NSString; external name '_NSAccessibilityApplicationRole';
  NSAccessibilityGroupRole: NSString; external name '_NSAccessibilityGroupRole';
  NSAccessibilityRadioGroupRole: NSString; external name '_NSAccessibilityRadioGroupRole';
  NSAccessibilityListRole: NSString; external name '_NSAccessibilityListRole';
  NSAccessibilityScrollBarRole: NSString; external name '_NSAccessibilityScrollBarRole';
  NSAccessibilityValueIndicatorRole: NSString; external name '_NSAccessibilityValueIndicatorRole';
  NSAccessibilityImageRole: NSString; external name '_NSAccessibilityImageRole';
  NSAccessibilityMenuBarRole: NSString; external name '_NSAccessibilityMenuBarRole';
  NSAccessibilityMenuRole: NSString; external name '_NSAccessibilityMenuRole';
  NSAccessibilityMenuItemRole: NSString; external name '_NSAccessibilityMenuItemRole';
  NSAccessibilityColumnRole: NSString; external name '_NSAccessibilityColumnRole';
  NSAccessibilityRowRole: NSString; external name '_NSAccessibilityRowRole';
  NSAccessibilityToolbarRole: NSString; external name '_NSAccessibilityToolbarRole';
  NSAccessibilityBusyIndicatorRole: NSString; external name '_NSAccessibilityBusyIndicatorRole';
  NSAccessibilityProgressIndicatorRole: NSString; external name '_NSAccessibilityProgressIndicatorRole';
  NSAccessibilityWindowRole: NSString; external name '_NSAccessibilityWindowRole';
  NSAccessibilityDrawerRole: NSString; external name '_NSAccessibilityDrawerRole';
  NSAccessibilitySystemWideRole: NSString; external name '_NSAccessibilitySystemWideRole';
  NSAccessibilityOutlineRole: NSString; external name '_NSAccessibilityOutlineRole';
  NSAccessibilityIncrementorRole: NSString; external name '_NSAccessibilityIncrementorRole';
  NSAccessibilityBrowserRole: NSString; external name '_NSAccessibilityBrowserRole';
  NSAccessibilityComboBoxRole: NSString; external name '_NSAccessibilityComboBoxRole';
  NSAccessibilitySplitGroupRole: NSString; external name '_NSAccessibilitySplitGroupRole';
  NSAccessibilitySplitterRole: NSString; external name '_NSAccessibilitySplitterRole';
  NSAccessibilityColorWellRole: NSString; external name '_NSAccessibilityColorWellRole';
  NSAccessibilityGrowAreaRole: NSString; external name '_NSAccessibilityGrowAreaRole';
  NSAccessibilitySheetRole: NSString; external name '_NSAccessibilitySheetRole';
  NSAccessibilityHelpTagRole: NSString; external name '_NSAccessibilityHelpTagRole';
  NSAccessibilityMatteRole: NSString; external name '_NSAccessibilityMatteRole';
  NSAccessibilityRulerRole: NSString; external name '_NSAccessibilityRulerRole';
  NSAccessibilityRulerMarkerRole: NSString; external name '_NSAccessibilityRulerMarkerRole';
  NSAccessibilitySortButtonRole: NSString; external name '_NSAccessibilitySortButtonRole';
  NSAccessibilityLinkRole: NSString; external name '_NSAccessibilityLinkRole';
  NSAccessibilityDisclosureTriangleRole: NSString; external name '_NSAccessibilityDisclosureTriangleRole';
  NSAccessibilityGridRole: NSString; external name '_NSAccessibilityGridRole';
  NSAccessibilityUnknownSubrole: NSString; external name '_NSAccessibilityUnknownSubrole';
  NSAccessibilityCloseButtonSubrole: NSString; external name '_NSAccessibilityCloseButtonSubrole';
  NSAccessibilityZoomButtonSubrole: NSString; external name '_NSAccessibilityZoomButtonSubrole';
  NSAccessibilityMinimizeButtonSubrole: NSString; external name '_NSAccessibilityMinimizeButtonSubrole';
  NSAccessibilityToolbarButtonSubrole: NSString; external name '_NSAccessibilityToolbarButtonSubrole';
  NSAccessibilityTableRowSubrole: NSString; external name '_NSAccessibilityTableRowSubrole';
  NSAccessibilityOutlineRowSubrole: NSString; external name '_NSAccessibilityOutlineRowSubrole';
  NSAccessibilitySecureTextFieldSubrole: NSString; external name '_NSAccessibilitySecureTextFieldSubrole';
  NSAccessibilityStandardWindowSubrole: NSString; external name '_NSAccessibilityStandardWindowSubrole';
  NSAccessibilityDialogSubrole: NSString; external name '_NSAccessibilityDialogSubrole';
  NSAccessibilitySystemDialogSubrole: NSString; external name '_NSAccessibilitySystemDialogSubrole';
  NSAccessibilityFloatingWindowSubrole: NSString; external name '_NSAccessibilityFloatingWindowSubrole';
  NSAccessibilitySystemFloatingWindowSubrole: NSString; external name '_NSAccessibilitySystemFloatingWindowSubrole';
  NSAccessibilityIncrementArrowSubrole: NSString; external name '_NSAccessibilityIncrementArrowSubrole';
  NSAccessibilityDecrementArrowSubrole: NSString; external name '_NSAccessibilityDecrementArrowSubrole';
  NSAccessibilityIncrementPageSubrole: NSString; external name '_NSAccessibilityIncrementPageSubrole';
  NSAccessibilityDecrementPageSubrole: NSString; external name '_NSAccessibilityDecrementPageSubrole';
  NSAccessibilitySearchFieldSubrole: NSString; external name '_NSAccessibilitySearchFieldSubrole';
  NSAccessibilityTextAttachmentSubrole: NSString; external name '_NSAccessibilityTextAttachmentSubrole';
  NSAccessibilityTextLinkSubrole: NSString; external name '_NSAccessibilityTextLinkSubrole';
  NSAccessibilityTimelineSubrole: NSString; external name '_NSAccessibilityTimelineSubrole';

{$endif}
{$endif}

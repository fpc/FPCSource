{ Parsed from Appkit.framework NSAccessibility.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSACCESSIBILITY_PAS_T}
{$define NSACCESSIBILITY_PAS_T}

{ CFString constants }
var
  NSAccessibilityErrorCodeExceptionInfo: CFStringRef; external name '_NSAccessibilityErrorCodeExceptionInfo';
  NSAccessibilityRoleAttribute: CFStringRef; external name '_NSAccessibilityRoleAttribute';
  NSAccessibilityRoleDescriptionAttribute: CFStringRef; external name '_NSAccessibilityRoleDescriptionAttribute';
  NSAccessibilitySubroleAttribute: CFStringRef; external name '_NSAccessibilitySubroleAttribute';
  NSAccessibilityHelpAttribute: CFStringRef; external name '_NSAccessibilityHelpAttribute';
  NSAccessibilityValueAttribute: CFStringRef; external name '_NSAccessibilityValueAttribute';
  NSAccessibilityMinValueAttribute: CFStringRef; external name '_NSAccessibilityMinValueAttribute';
  NSAccessibilityMaxValueAttribute: CFStringRef; external name '_NSAccessibilityMaxValueAttribute';
  NSAccessibilityEnabledAttribute: CFStringRef; external name '_NSAccessibilityEnabledAttribute';
  NSAccessibilityFocusedAttribute: CFStringRef; external name '_NSAccessibilityFocusedAttribute';
  NSAccessibilityParentAttribute: CFStringRef; external name '_NSAccessibilityParentAttribute';
  NSAccessibilityChildrenAttribute: CFStringRef; external name '_NSAccessibilityChildrenAttribute';
  NSAccessibilityWindowAttribute: CFStringRef; external name '_NSAccessibilityWindowAttribute';
  NSAccessibilitySelectedChildrenAttribute: CFStringRef; external name '_NSAccessibilitySelectedChildrenAttribute';
  NSAccessibilityVisibleChildrenAttribute: CFStringRef; external name '_NSAccessibilityVisibleChildrenAttribute';
  NSAccessibilityPositionAttribute: CFStringRef; external name '_NSAccessibilityPositionAttribute';
  NSAccessibilitySizeAttribute: CFStringRef; external name '_NSAccessibilitySizeAttribute';
  NSAccessibilityContentsAttribute: CFStringRef; external name '_NSAccessibilityContentsAttribute';
  NSAccessibilityTitleAttribute: CFStringRef; external name '_NSAccessibilityTitleAttribute';
  NSAccessibilityPreviousContentsAttribute: CFStringRef; external name '_NSAccessibilityPreviousContentsAttribute';
  NSAccessibilityNextContentsAttribute: CFStringRef; external name '_NSAccessibilityNextContentsAttribute';
  NSAccessibilityHeaderAttribute: CFStringRef; external name '_NSAccessibilityHeaderAttribute';
  NSAccessibilityEditedAttribute: CFStringRef; external name '_NSAccessibilityEditedAttribute';
  NSAccessibilityTabsAttribute: CFStringRef; external name '_NSAccessibilityTabsAttribute';
  NSAccessibilityHorizontalScrollBarAttribute: CFStringRef; external name '_NSAccessibilityHorizontalScrollBarAttribute';
  NSAccessibilityVerticalScrollBarAttribute: CFStringRef; external name '_NSAccessibilityVerticalScrollBarAttribute';
  NSAccessibilityOverflowButtonAttribute: CFStringRef; external name '_NSAccessibilityOverflowButtonAttribute';
  NSAccessibilityIncrementButtonAttribute: CFStringRef; external name '_NSAccessibilityIncrementButtonAttribute';
  NSAccessibilityDecrementButtonAttribute: CFStringRef; external name '_NSAccessibilityDecrementButtonAttribute';
  NSAccessibilityFilenameAttribute: CFStringRef; external name '_NSAccessibilityFilenameAttribute';
  NSAccessibilityExpandedAttribute: CFStringRef; external name '_NSAccessibilityExpandedAttribute';
  NSAccessibilitySelectedAttribute: CFStringRef; external name '_NSAccessibilitySelectedAttribute';
  NSAccessibilitySplittersAttribute: CFStringRef; external name '_NSAccessibilitySplittersAttribute';
  NSAccessibilityDocumentAttribute: CFStringRef; external name '_NSAccessibilityDocumentAttribute';
  NSAccessibilityTitleUIElementAttribute: CFStringRef; external name '_NSAccessibilityTitleUIElementAttribute';
  NSAccessibilitySelectedTextAttribute: CFStringRef; external name '_NSAccessibilitySelectedTextAttribute';
  NSAccessibilitySelectedTextRangeAttribute: CFStringRef; external name '_NSAccessibilitySelectedTextRangeAttribute';
  NSAccessibilityMainAttribute: CFStringRef; external name '_NSAccessibilityMainAttribute';
  NSAccessibilityMinimizedAttribute: CFStringRef; external name '_NSAccessibilityMinimizedAttribute';
  NSAccessibilityCloseButtonAttribute: CFStringRef; external name '_NSAccessibilityCloseButtonAttribute';
  NSAccessibilityZoomButtonAttribute: CFStringRef; external name '_NSAccessibilityZoomButtonAttribute';
  NSAccessibilityMinimizeButtonAttribute: CFStringRef; external name '_NSAccessibilityMinimizeButtonAttribute';
  NSAccessibilityToolbarButtonAttribute: CFStringRef; external name '_NSAccessibilityToolbarButtonAttribute';
  NSAccessibilityProxyAttribute: CFStringRef; external name '_NSAccessibilityProxyAttribute';
  NSAccessibilityGrowAreaAttribute: CFStringRef; external name '_NSAccessibilityGrowAreaAttribute';
  NSAccessibilityMenuBarAttribute: CFStringRef; external name '_NSAccessibilityMenuBarAttribute';
  NSAccessibilityWindowsAttribute: CFStringRef; external name '_NSAccessibilityWindowsAttribute';
  NSAccessibilityFrontmostAttribute: CFStringRef; external name '_NSAccessibilityFrontmostAttribute';
  NSAccessibilityHiddenAttribute: CFStringRef; external name '_NSAccessibilityHiddenAttribute';
  NSAccessibilityMainWindowAttribute: CFStringRef; external name '_NSAccessibilityMainWindowAttribute';
  NSAccessibilityFocusedWindowAttribute: CFStringRef; external name '_NSAccessibilityFocusedWindowAttribute';
  NSAccessibilityFocusedUIElementAttribute: CFStringRef; external name '_NSAccessibilityFocusedUIElementAttribute';
  NSAccessibilityOrientationAttribute: CFStringRef; external name '_NSAccessibilityOrientationAttribute';
  NSAccessibilityVerticalOrientationValue: CFStringRef; external name '_NSAccessibilityVerticalOrientationValue';
  NSAccessibilityHorizontalOrientationValue: CFStringRef; external name '_NSAccessibilityHorizontalOrientationValue';
  NSAccessibilityColumnTitlesAttribute: CFStringRef; external name '_NSAccessibilityColumnTitlesAttribute';
  NSAccessibilityRowsAttribute: CFStringRef; external name '_NSAccessibilityRowsAttribute';
  NSAccessibilityVisibleRowsAttribute: CFStringRef; external name '_NSAccessibilityVisibleRowsAttribute';
  NSAccessibilitySelectedRowsAttribute: CFStringRef; external name '_NSAccessibilitySelectedRowsAttribute';
  NSAccessibilityColumnsAttribute: CFStringRef; external name '_NSAccessibilityColumnsAttribute';
  NSAccessibilityVisibleColumnsAttribute: CFStringRef; external name '_NSAccessibilityVisibleColumnsAttribute';
  NSAccessibilitySelectedColumnsAttribute: CFStringRef; external name '_NSAccessibilitySelectedColumnsAttribute';
  NSAccessibilityDisclosingAttribute: CFStringRef; external name '_NSAccessibilityDisclosingAttribute';
  NSAccessibilityDisclosedRowsAttribute: CFStringRef; external name '_NSAccessibilityDisclosedRowsAttribute';
  NSAccessibilityDisclosedByRowAttribute: CFStringRef; external name '_NSAccessibilityDisclosedByRowAttribute';
  NSAccessibilityPressAction: CFStringRef; external name '_NSAccessibilityPressAction';
  NSAccessibilityIncrementAction: CFStringRef; external name '_NSAccessibilityIncrementAction';
  NSAccessibilityDecrementAction: CFStringRef; external name '_NSAccessibilityDecrementAction';
  NSAccessibilityConfirmAction: CFStringRef; external name '_NSAccessibilityConfirmAction';
  NSAccessibilityPickAction: CFStringRef; external name '_NSAccessibilityPickAction';
  NSAccessibilityMainWindowChangedNotification: CFStringRef; external name '_NSAccessibilityMainWindowChangedNotification';
  NSAccessibilityFocusedWindowChangedNotification: CFStringRef; external name '_NSAccessibilityFocusedWindowChangedNotification';
  NSAccessibilityFocusedUIElementChangedNotification: CFStringRef; external name '_NSAccessibilityFocusedUIElementChangedNotification';
  NSAccessibilityApplicationActivatedNotification: CFStringRef; external name '_NSAccessibilityApplicationActivatedNotification';
  NSAccessibilityApplicationDeactivatedNotification: CFStringRef; external name '_NSAccessibilityApplicationDeactivatedNotification';
  NSAccessibilityApplicationHiddenNotification: CFStringRef; external name '_NSAccessibilityApplicationHiddenNotification';
  NSAccessibilityApplicationShownNotification: CFStringRef; external name '_NSAccessibilityApplicationShownNotification';
  NSAccessibilityWindowCreatedNotification: CFStringRef; external name '_NSAccessibilityWindowCreatedNotification';
  NSAccessibilityWindowMovedNotification: CFStringRef; external name '_NSAccessibilityWindowMovedNotification';
  NSAccessibilityWindowResizedNotification: CFStringRef; external name '_NSAccessibilityWindowResizedNotification';
  NSAccessibilityWindowMiniaturizedNotification: CFStringRef; external name '_NSAccessibilityWindowMiniaturizedNotification';
  NSAccessibilityWindowDeminiaturizedNotification: CFStringRef; external name '_NSAccessibilityWindowDeminiaturizedNotification';
  NSAccessibilityUIElementDestroyedNotification: CFStringRef; external name '_NSAccessibilityUIElementDestroyedNotification';
  NSAccessibilityValueChangedNotification: CFStringRef; external name '_NSAccessibilityValueChangedNotification';
  NSAccessibilityUnknownRole: CFStringRef; external name '_NSAccessibilityUnknownRole';
  NSAccessibilityButtonRole: CFStringRef; external name '_NSAccessibilityButtonRole';
  NSAccessibilityRadioButtonRole: CFStringRef; external name '_NSAccessibilityRadioButtonRole';
  NSAccessibilityCheckBoxRole: CFStringRef; external name '_NSAccessibilityCheckBoxRole';
  NSAccessibilitySliderRole: CFStringRef; external name '_NSAccessibilitySliderRole';
  NSAccessibilityTabGroupRole: CFStringRef; external name '_NSAccessibilityTabGroupRole';
  NSAccessibilityTextFieldRole: CFStringRef; external name '_NSAccessibilityTextFieldRole';
  NSAccessibilityStaticTextRole: CFStringRef; external name '_NSAccessibilityStaticTextRole';
  NSAccessibilityTextAreaRole: CFStringRef; external name '_NSAccessibilityTextAreaRole';
  NSAccessibilityScrollAreaRole: CFStringRef; external name '_NSAccessibilityScrollAreaRole';
  NSAccessibilityPopUpButtonRole: CFStringRef; external name '_NSAccessibilityPopUpButtonRole';
  NSAccessibilityMenuButtonRole: CFStringRef; external name '_NSAccessibilityMenuButtonRole';
  NSAccessibilityTableRole: CFStringRef; external name '_NSAccessibilityTableRole';
  NSAccessibilityApplicationRole: CFStringRef; external name '_NSAccessibilityApplicationRole';
  NSAccessibilityGroupRole: CFStringRef; external name '_NSAccessibilityGroupRole';
  NSAccessibilityRadioGroupRole: CFStringRef; external name '_NSAccessibilityRadioGroupRole';
  NSAccessibilityListRole: CFStringRef; external name '_NSAccessibilityListRole';
  NSAccessibilityScrollBarRole: CFStringRef; external name '_NSAccessibilityScrollBarRole';
  NSAccessibilityValueIndicatorRole: CFStringRef; external name '_NSAccessibilityValueIndicatorRole';
  NSAccessibilityImageRole: CFStringRef; external name '_NSAccessibilityImageRole';
  NSAccessibilityMenuBarRole: CFStringRef; external name '_NSAccessibilityMenuBarRole';
  NSAccessibilityMenuRole: CFStringRef; external name '_NSAccessibilityMenuRole';
  NSAccessibilityMenuItemRole: CFStringRef; external name '_NSAccessibilityMenuItemRole';
  NSAccessibilityColumnRole: CFStringRef; external name '_NSAccessibilityColumnRole';
  NSAccessibilityRowRole: CFStringRef; external name '_NSAccessibilityRowRole';
  NSAccessibilityToolbarRole: CFStringRef; external name '_NSAccessibilityToolbarRole';
  NSAccessibilityBusyIndicatorRole: CFStringRef; external name '_NSAccessibilityBusyIndicatorRole';
  NSAccessibilityProgressIndicatorRole: CFStringRef; external name '_NSAccessibilityProgressIndicatorRole';
  NSAccessibilityWindowRole: CFStringRef; external name '_NSAccessibilityWindowRole';
  NSAccessibilityDrawerRole: CFStringRef; external name '_NSAccessibilityDrawerRole';
  NSAccessibilitySystemWideRole: CFStringRef; external name '_NSAccessibilitySystemWideRole';
  NSAccessibilityOutlineRole: CFStringRef; external name '_NSAccessibilityOutlineRole';
  NSAccessibilityIncrementorRole: CFStringRef; external name '_NSAccessibilityIncrementorRole';
  NSAccessibilityBrowserRole: CFStringRef; external name '_NSAccessibilityBrowserRole';
  NSAccessibilityComboBoxRole: CFStringRef; external name '_NSAccessibilityComboBoxRole';
  NSAccessibilitySplitGroupRole: CFStringRef; external name '_NSAccessibilitySplitGroupRole';
  NSAccessibilitySplitterRole: CFStringRef; external name '_NSAccessibilitySplitterRole';
  NSAccessibilityColorWellRole: CFStringRef; external name '_NSAccessibilityColorWellRole';
  NSAccessibilityGrowAreaRole: CFStringRef; external name '_NSAccessibilityGrowAreaRole';
  NSAccessibilitySheetRole: CFStringRef; external name '_NSAccessibilitySheetRole';
  NSAccessibilityUnknownSubrole: CFStringRef; external name '_NSAccessibilityUnknownSubrole';
  NSAccessibilityCloseButtonSubrole: CFStringRef; external name '_NSAccessibilityCloseButtonSubrole';
  NSAccessibilityZoomButtonSubrole: CFStringRef; external name '_NSAccessibilityZoomButtonSubrole';
  NSAccessibilityMinimizeButtonSubrole: CFStringRef; external name '_NSAccessibilityMinimizeButtonSubrole';
  NSAccessibilityToolbarButtonSubrole: CFStringRef; external name '_NSAccessibilityToolbarButtonSubrole';
  NSAccessibilityTableRowSubrole: CFStringRef; external name '_NSAccessibilityTableRowSubrole';
  NSAccessibilityOutlineRowSubrole: CFStringRef; external name '_NSAccessibilityOutlineRowSubrole';
  NSAccessibilitySecureTextFieldSubrole: CFStringRef; external name '_NSAccessibilitySecureTextFieldSubrole';

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
function NSAccessibilityRoleDescription(var role: NSString; var subrole: NSString): NSString; cdecl; external name 'NSAccessibilityRoleDescription';
function NSAccessibilityRoleDescriptionForUIElement(element: id): NSString; cdecl; external name 'NSAccessibilityRoleDescriptionForUIElement';
function NSAccessibilityActionDescription(var action: NSString): NSString; cdecl; external name 'NSAccessibilityActionDescription';
procedure NSAccessibilityRaiseBadArgumentException(element: id; var attribute: NSString; value: id); cdecl; external name 'NSAccessibilityRaiseBadArgumentException';
function NSAccessibilityUnignoredAncestor(element: id): id; cdecl; external name 'NSAccessibilityUnignoredAncestor';
function NSAccessibilityUnignoredDescendant(element: id): id; cdecl; external name 'NSAccessibilityUnignoredDescendant';
function NSAccessibilityUnignoredChildren(var originalChildren: NSArray): NSArray; cdecl; external name 'NSAccessibilityUnignoredChildren';
function NSAccessibilityUnignoredChildrenForOnlyChild(originalChild: id): NSArray; cdecl; external name 'NSAccessibilityUnignoredChildrenForOnlyChild';
procedure NSAccessibilityPostNotification(element: id; var notification: NSString); cdecl; external name 'NSAccessibilityPostNotification';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSACCESSIBILITY_PAS_T}
{$define NSACCESSIBILITY_PAS_T}

{$endif}
{$endif}

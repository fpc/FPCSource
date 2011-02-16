{ Parsed from Appkit.framework NSAccessibility.h }

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
function NSAccessibilityRoleDescription(role: NSString; subrole: NSString): NSString; cdecl; external;
function NSAccessibilityRoleDescriptionForUIElement(element: id): NSString; cdecl; external;
function NSAccessibilityActionDescription(action: NSString): NSString; cdecl; external;
procedure NSAccessibilityRaiseBadArgumentException(element: id; attribute: NSString; value: id); cdecl; external;
function NSAccessibilityUnignoredAncestor(element: id): id; cdecl; external;
function NSAccessibilityUnignoredDescendant(element: id): id; cdecl; external;
function NSAccessibilityUnignoredChildren(originalChildren: NSArray): NSArray; cdecl; external;
function NSAccessibilityUnignoredChildrenForOnlyChild(originalChild: id): NSArray; cdecl; external;
procedure NSAccessibilityPostNotification(element: id; notification: NSString); cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSACCESSIBILITY_PAS_S}
{$define NSACCESSIBILITY_PAS_S}

{ External string constants }
var
  NSAccessibilityErrorCodeExceptionInfo: NSString; cvar; external;
  NSAccessibilityRoleAttribute: NSString; cvar; external;
  NSAccessibilityRoleDescriptionAttribute: NSString; cvar; external;
  NSAccessibilitySubroleAttribute: NSString; cvar; external;
  NSAccessibilityHelpAttribute: NSString; cvar; external;
  NSAccessibilityValueAttribute: NSString; cvar; external;
  NSAccessibilityMinValueAttribute: NSString; cvar; external;
  NSAccessibilityMaxValueAttribute: NSString; cvar; external;
  NSAccessibilityEnabledAttribute: NSString; cvar; external;
  NSAccessibilityFocusedAttribute: NSString; cvar; external;
  NSAccessibilityParentAttribute: NSString; cvar; external;
  NSAccessibilityChildrenAttribute: NSString; cvar; external;
  NSAccessibilityWindowAttribute: NSString; cvar; external;
  NSAccessibilityTopLevelUIElementAttribute: NSString; cvar; external;
  NSAccessibilitySelectedChildrenAttribute: NSString; cvar; external;
  NSAccessibilityVisibleChildrenAttribute: NSString; cvar; external;
  NSAccessibilityPositionAttribute: NSString; cvar; external;
  NSAccessibilitySizeAttribute: NSString; cvar; external;
  NSAccessibilityContentsAttribute: NSString; cvar; external;
  NSAccessibilityTitleAttribute: NSString; cvar; external;
  NSAccessibilityDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityShownMenuAttribute: NSString; cvar; external;
  NSAccessibilityValueDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityPreviousContentsAttribute: NSString; cvar; external;
  NSAccessibilityNextContentsAttribute: NSString; cvar; external;
  NSAccessibilityHeaderAttribute: NSString; cvar; external;
  NSAccessibilityEditedAttribute: NSString; cvar; external;
  NSAccessibilityTabsAttribute: NSString; cvar; external;
  NSAccessibilityHorizontalScrollBarAttribute: NSString; cvar; external;
  NSAccessibilityVerticalScrollBarAttribute: NSString; cvar; external;
  NSAccessibilityOverflowButtonAttribute: NSString; cvar; external;
  NSAccessibilityIncrementButtonAttribute: NSString; cvar; external;
  NSAccessibilityDecrementButtonAttribute: NSString; cvar; external;
  NSAccessibilityFilenameAttribute: NSString; cvar; external;
  NSAccessibilityExpandedAttribute: NSString; cvar; external;
  NSAccessibilitySelectedAttribute: NSString; cvar; external;
  NSAccessibilitySplittersAttribute: NSString; cvar; external;
  NSAccessibilityDocumentAttribute: NSString; cvar; external;
  NSAccessibilityURLAttribute: NSString; cvar; external;
  NSAccessibilityIndexAttribute: NSString; cvar; external;
  NSAccessibilityRowCountAttribute: NSString; cvar; external;
  NSAccessibilityColumnCountAttribute: NSString; cvar; external;
  NSAccessibilityOrderedByRowAttribute: NSString; cvar; external;
  NSAccessibilityWarningValueAttribute: NSString; cvar; external;
  NSAccessibilityCriticalValueAttribute: NSString; cvar; external;
  NSAccessibilityPlaceholderValueAttribute: NSString; cvar; external;
  NSAccessibilityTitleUIElementAttribute: NSString; cvar; external;
  NSAccessibilityServesAsTitleForUIElementsAttribute: NSString; cvar; external;
  NSAccessibilityLinkedUIElementsAttribute: NSString; cvar; external;
  NSAccessibilitySelectedTextAttribute: NSString; cvar; external;
  NSAccessibilitySelectedTextRangeAttribute: NSString; cvar; external;
  NSAccessibilityNumberOfCharactersAttribute: NSString; cvar; external;
  NSAccessibilityVisibleCharacterRangeAttribute: NSString; cvar; external;
  NSAccessibilitySharedTextUIElementsAttribute: NSString; cvar; external;
  NSAccessibilitySharedCharacterRangeAttribute: NSString; cvar; external;
  NSAccessibilityInsertionPointLineNumberAttribute: NSString; cvar; external;
  NSAccessibilitySelectedTextRangesAttribute: NSString; cvar; external;
  NSAccessibilityLineForIndexParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityRangeForLineParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityStringForRangeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityRangeForPositionParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityRangeForIndexParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityBoundsForRangeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityRTFForRangeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityStyleRangeForIndexParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityAttributedStringForRangeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityFontTextAttribute: NSString; cvar; external;
  NSAccessibilityForegroundColorTextAttribute: NSString; cvar; external;
  NSAccessibilityBackgroundColorTextAttribute: NSString; cvar; external;
  NSAccessibilityUnderlineColorTextAttribute: NSString; cvar; external;
  NSAccessibilityStrikethroughColorTextAttribute: NSString; cvar; external;
  NSAccessibilityUnderlineTextAttribute: NSString; cvar; external;
  NSAccessibilitySuperscriptTextAttribute: NSString; cvar; external;
  NSAccessibilityStrikethroughTextAttribute: NSString; cvar; external;
  NSAccessibilityShadowTextAttribute: NSString; cvar; external;
  NSAccessibilityAttachmentTextAttribute: NSString; cvar; external;
  NSAccessibilityLinkTextAttribute: NSString; cvar; external;
  NSAccessibilityMisspelledTextAttribute: NSString; cvar; external;
  NSAccessibilityFontNameKey: NSString; cvar; external;
  NSAccessibilityFontFamilyKey: NSString; cvar; external;
  NSAccessibilityVisibleNameKey: NSString; cvar; external;
  NSAccessibilityFontSizeKey: NSString; cvar; external;
  NSAccessibilityMainAttribute: NSString; cvar; external;
  NSAccessibilityMinimizedAttribute: NSString; cvar; external;
  NSAccessibilityCloseButtonAttribute: NSString; cvar; external;
  NSAccessibilityZoomButtonAttribute: NSString; cvar; external;
  NSAccessibilityMinimizeButtonAttribute: NSString; cvar; external;
  NSAccessibilityToolbarButtonAttribute: NSString; cvar; external;
  NSAccessibilityProxyAttribute: NSString; cvar; external;
  NSAccessibilityGrowAreaAttribute: NSString; cvar; external;
  NSAccessibilityModalAttribute: NSString; cvar; external;
  NSAccessibilityDefaultButtonAttribute: NSString; cvar; external;
  NSAccessibilityCancelButtonAttribute: NSString; cvar; external;
  NSAccessibilityMenuBarAttribute: NSString; cvar; external;
  NSAccessibilityWindowsAttribute: NSString; cvar; external;
  NSAccessibilityFrontmostAttribute: NSString; cvar; external;
  NSAccessibilityHiddenAttribute: NSString; cvar; external;
  NSAccessibilityMainWindowAttribute: NSString; cvar; external;
  NSAccessibilityFocusedWindowAttribute: NSString; cvar; external;
  NSAccessibilityFocusedUIElementAttribute: NSString; cvar; external;
  NSAccessibilityOrientationAttribute: NSString; cvar; external;
  NSAccessibilityVerticalOrientationValue: NSString; cvar; external;
  NSAccessibilityHorizontalOrientationValue: NSString; cvar; external;
  NSAccessibilityUnknownOrientationValue: NSString; cvar; external;
  NSAccessibilityColumnTitlesAttribute: NSString; cvar; external;
  NSAccessibilitySearchButtonAttribute: NSString; cvar; external;
  NSAccessibilitySearchMenuAttribute: NSString; cvar; external;
  NSAccessibilityClearButtonAttribute: NSString; cvar; external;
  NSAccessibilityRowsAttribute: NSString; cvar; external;
  NSAccessibilityVisibleRowsAttribute: NSString; cvar; external;
  NSAccessibilitySelectedRowsAttribute: NSString; cvar; external;
  NSAccessibilityColumnsAttribute: NSString; cvar; external;
  NSAccessibilityVisibleColumnsAttribute: NSString; cvar; external;
  NSAccessibilitySelectedColumnsAttribute: NSString; cvar; external;
  NSAccessibilitySortDirectionAttribute: NSString; cvar; external;
  NSAccessibilitySelectedCellsAttribute: NSString; cvar; external;
  NSAccessibilityVisibleCellsAttribute: NSString; cvar; external;
  NSAccessibilityRowHeaderUIElementsAttribute: NSString; cvar; external;
  NSAccessibilityColumnHeaderUIElementsAttribute: NSString; cvar; external;
  NSAccessibilityCellForColumnAndRowParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityRowIndexRangeAttribute: NSString; cvar; external;
  NSAccessibilityColumnIndexRangeAttribute: NSString; cvar; external;
  NSAccessibilityHorizontalUnitsAttribute: NSString; cvar; external;
  NSAccessibilityVerticalUnitsAttribute: NSString; cvar; external;
  NSAccessibilityHorizontalUnitDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityVerticalUnitDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityLayoutPointForScreenPointParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityLayoutSizeForScreenSizeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityScreenPointForLayoutPointParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityScreenSizeForLayoutSizeParameterizedAttribute: NSString; cvar; external;
  NSAccessibilityHandlesAttribute: NSString; cvar; external;
  NSAccessibilityAscendingSortDirectionValue: NSString; cvar; external;
  NSAccessibilityDescendingSortDirectionValue: NSString; cvar; external;
  NSAccessibilityUnknownSortDirectionValue: NSString; cvar; external;
  NSAccessibilityDisclosingAttribute: NSString; cvar; external;
  NSAccessibilityDisclosedRowsAttribute: NSString; cvar; external;
  NSAccessibilityDisclosedByRowAttribute: NSString; cvar; external;
  NSAccessibilityDisclosureLevelAttribute: NSString; cvar; external;
  NSAccessibilityAllowedValuesAttribute: NSString; cvar; external;
  NSAccessibilityLabelUIElementsAttribute: NSString; cvar; external;
  NSAccessibilityLabelValueAttribute: NSString; cvar; external;
  NSAccessibilityMatteHoleAttribute: NSString; cvar; external;
  NSAccessibilityMatteContentUIElementAttribute: NSString; cvar; external;
  NSAccessibilityMarkerUIElementsAttribute: NSString; cvar; external;
  NSAccessibilityMarkerValuesAttribute: NSString; cvar; external;
  NSAccessibilityMarkerGroupUIElementAttribute: NSString; cvar; external;
  NSAccessibilityUnitsAttribute: NSString; cvar; external;
  NSAccessibilityUnitDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityMarkerTypeAttribute: NSString; cvar; external;
  NSAccessibilityMarkerTypeDescriptionAttribute: NSString; cvar; external;
  NSAccessibilityLeftTabStopMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityRightTabStopMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityCenterTabStopMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityDecimalTabStopMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityHeadIndentMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityTailIndentMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityFirstLineIndentMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityUnknownMarkerTypeValue: NSString; cvar; external;
  NSAccessibilityInchesUnitValue: NSString; cvar; external;
  NSAccessibilityCentimetersUnitValue: NSString; cvar; external;
  NSAccessibilityPointsUnitValue: NSString; cvar; external;
  NSAccessibilityPicasUnitValue: NSString; cvar; external;
  NSAccessibilityUnknownUnitValue: NSString; cvar; external;
  NSAccessibilityPressAction: NSString; cvar; external;
  NSAccessibilityIncrementAction: NSString; cvar; external;
  NSAccessibilityDecrementAction: NSString; cvar; external;
  NSAccessibilityConfirmAction: NSString; cvar; external;
  NSAccessibilityPickAction: NSString; cvar; external;
  NSAccessibilityCancelAction: NSString; cvar; external;
  NSAccessibilityRaiseAction: NSString; cvar; external;
  NSAccessibilityShowMenuAction: NSString; cvar; external;
  NSAccessibilityDeleteAction: NSString; cvar; external;
  NSAccessibilityMainWindowChangedNotification: NSString; cvar; external;
  NSAccessibilityFocusedWindowChangedNotification: NSString; cvar; external;
  NSAccessibilityFocusedUIElementChangedNotification: NSString; cvar; external;
  NSAccessibilityApplicationActivatedNotification: NSString; cvar; external;
  NSAccessibilityApplicationDeactivatedNotification: NSString; cvar; external;
  NSAccessibilityApplicationHiddenNotification: NSString; cvar; external;
  NSAccessibilityApplicationShownNotification: NSString; cvar; external;
  NSAccessibilityWindowCreatedNotification: NSString; cvar; external;
  NSAccessibilityWindowMovedNotification: NSString; cvar; external;
  NSAccessibilityWindowResizedNotification: NSString; cvar; external;
  NSAccessibilityWindowMiniaturizedNotification: NSString; cvar; external;
  NSAccessibilityWindowDeminiaturizedNotification: NSString; cvar; external;
  NSAccessibilityDrawerCreatedNotification: NSString; cvar; external;
  NSAccessibilitySheetCreatedNotification: NSString; cvar; external;
  NSAccessibilityUIElementDestroyedNotification: NSString; cvar; external;
  NSAccessibilityValueChangedNotification: NSString; cvar; external;
  NSAccessibilityTitleChangedNotification: NSString; cvar; external;
  NSAccessibilityResizedNotification: NSString; cvar; external;
  NSAccessibilityMovedNotification: NSString; cvar; external;
  NSAccessibilityCreatedNotification: NSString; cvar; external;
  NSAccessibilityHelpTagCreatedNotification: NSString; cvar; external;
  NSAccessibilitySelectedTextChangedNotification: NSString; cvar; external;
  NSAccessibilityRowCountChangedNotification: NSString; cvar; external;
  NSAccessibilitySelectedChildrenChangedNotification: NSString; cvar; external;
  NSAccessibilitySelectedRowsChangedNotification: NSString; cvar; external;
  NSAccessibilitySelectedColumnsChangedNotification: NSString; cvar; external;
  NSAccessibilityRowExpandedNotification: NSString; cvar; external;
  NSAccessibilityRowCollapsedNotification: NSString; cvar; external;
  NSAccessibilitySelectedCellsChangedNotification: NSString; cvar; external;
  NSAccessibilityUnitsChangedNotification: NSString; cvar; external;
  NSAccessibilitySelectedChildrenMovedNotification: NSString; cvar; external;
  NSAccessibilityUnknownRole: NSString; cvar; external;
  NSAccessibilityButtonRole: NSString; cvar; external;
  NSAccessibilityRadioButtonRole: NSString; cvar; external;
  NSAccessibilityCheckBoxRole: NSString; cvar; external;
  NSAccessibilitySliderRole: NSString; cvar; external;
  NSAccessibilityTabGroupRole: NSString; cvar; external;
  NSAccessibilityTextFieldRole: NSString; cvar; external;
  NSAccessibilityStaticTextRole: NSString; cvar; external;
  NSAccessibilityTextAreaRole: NSString; cvar; external;
  NSAccessibilityScrollAreaRole: NSString; cvar; external;
  NSAccessibilityPopUpButtonRole: NSString; cvar; external;
  NSAccessibilityMenuButtonRole: NSString; cvar; external;
  NSAccessibilityTableRole: NSString; cvar; external;
  NSAccessibilityApplicationRole: NSString; cvar; external;
  NSAccessibilityGroupRole: NSString; cvar; external;
  NSAccessibilityRadioGroupRole: NSString; cvar; external;
  NSAccessibilityListRole: NSString; cvar; external;
  NSAccessibilityScrollBarRole: NSString; cvar; external;
  NSAccessibilityValueIndicatorRole: NSString; cvar; external;
  NSAccessibilityImageRole: NSString; cvar; external;
  NSAccessibilityMenuBarRole: NSString; cvar; external;
  NSAccessibilityMenuRole: NSString; cvar; external;
  NSAccessibilityMenuItemRole: NSString; cvar; external;
  NSAccessibilityColumnRole: NSString; cvar; external;
  NSAccessibilityRowRole: NSString; cvar; external;
  NSAccessibilityToolbarRole: NSString; cvar; external;
  NSAccessibilityBusyIndicatorRole: NSString; cvar; external;
  NSAccessibilityProgressIndicatorRole: NSString; cvar; external;
  NSAccessibilityWindowRole: NSString; cvar; external;
  NSAccessibilityDrawerRole: NSString; cvar; external;
  NSAccessibilitySystemWideRole: NSString; cvar; external;
  NSAccessibilityOutlineRole: NSString; cvar; external;
  NSAccessibilityIncrementorRole: NSString; cvar; external;
  NSAccessibilityBrowserRole: NSString; cvar; external;
  NSAccessibilityComboBoxRole: NSString; cvar; external;
  NSAccessibilitySplitGroupRole: NSString; cvar; external;
  NSAccessibilitySplitterRole: NSString; cvar; external;
  NSAccessibilityColorWellRole: NSString; cvar; external;
  NSAccessibilityGrowAreaRole: NSString; cvar; external;
  NSAccessibilitySheetRole: NSString; cvar; external;
  NSAccessibilityHelpTagRole: NSString; cvar; external;
  NSAccessibilityMatteRole: NSString; cvar; external;
  NSAccessibilityRulerRole: NSString; cvar; external;
  NSAccessibilityRulerMarkerRole: NSString; cvar; external;
  NSAccessibilityLinkRole: NSString; cvar; external;
  NSAccessibilityDisclosureTriangleRole: NSString; cvar; external;
  NSAccessibilityGridRole: NSString; cvar; external;
  NSAccessibilityRelevanceIndicatorRole: NSString; cvar; external;
  NSAccessibilityLevelIndicatorRole: NSString; cvar; external;
  NSAccessibilityCellRole: NSString; cvar; external;
  NSAccessibilityLayoutAreaRole: NSString; cvar; external;
  NSAccessibilityLayoutItemRole: NSString; cvar; external;
  NSAccessibilityHandleRole: NSString; cvar; external;
  NSAccessibilityUnknownSubrole: NSString; cvar; external;
  NSAccessibilityCloseButtonSubrole: NSString; cvar; external;
  NSAccessibilityZoomButtonSubrole: NSString; cvar; external;
  NSAccessibilityMinimizeButtonSubrole: NSString; cvar; external;
  NSAccessibilityToolbarButtonSubrole: NSString; cvar; external;
  NSAccessibilityTableRowSubrole: NSString; cvar; external;
  NSAccessibilityOutlineRowSubrole: NSString; cvar; external;
  NSAccessibilitySecureTextFieldSubrole: NSString; cvar; external;
  NSAccessibilityStandardWindowSubrole: NSString; cvar; external;
  NSAccessibilityDialogSubrole: NSString; cvar; external;
  NSAccessibilitySystemDialogSubrole: NSString; cvar; external;
  NSAccessibilityFloatingWindowSubrole: NSString; cvar; external;
  NSAccessibilitySystemFloatingWindowSubrole: NSString; cvar; external;
  NSAccessibilityIncrementArrowSubrole: NSString; cvar; external;
  NSAccessibilityDecrementArrowSubrole: NSString; cvar; external;
  NSAccessibilityIncrementPageSubrole: NSString; cvar; external;
  NSAccessibilityDecrementPageSubrole: NSString; cvar; external;
  NSAccessibilitySearchFieldSubrole: NSString; cvar; external;
  NSAccessibilityTextAttachmentSubrole: NSString; cvar; external;
  NSAccessibilityTextLinkSubrole: NSString; cvar; external;
  NSAccessibilityTimelineSubrole: NSString; cvar; external;
  NSAccessibilitySortButtonSubrole: NSString; cvar; external;
  NSAccessibilityRatingIndicatorSubrole: NSString; cvar; external;
  NSAccessibilityContentListSubrole: NSString; cvar; external;
  NSAccessibilityDefinitionListSubrole: NSString; cvar; external;
  NSAccessibilitySortButtonRole: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;
  NSAccessibilityHorizontialUnitsAttribute: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;
  NSAccessibilityHorizontialUnitDescriptionAttribute: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSACCESSIBILITY_PAS_C}
{$define NSACCESSIBILITY_PAS_C}

{ NSAccessibilityCategory }
  NSAccessibilityCategory = objccategory external (NSObject)
    function accessibilityAttributeNames: NSArray; message 'accessibilityAttributeNames';
    function accessibilityAttributeValue(attribute: NSString): id; message 'accessibilityAttributeValue:';
    function accessibilityIsAttributeSettable(attribute: NSString): Boolean; message 'accessibilityIsAttributeSettable:';
    procedure accessibilitySetValue_forAttribute(value: id; attribute: NSString); message 'accessibilitySetValue:forAttribute:';
    function accessibilityParameterizedAttributeNames: NSArray; message 'accessibilityParameterizedAttributeNames';
    function accessibilityAttributeValue_forParameter(attribute: NSString; parameter: id): id; message 'accessibilityAttributeValue:forParameter:';
    function accessibilityActionNames: NSArray; message 'accessibilityActionNames';
    function accessibilityActionDescription(action: NSString): NSString; message 'accessibilityActionDescription:';
    procedure accessibilityPerformAction(action: NSString); message 'accessibilityPerformAction:';
    function accessibilityIsIgnored: Boolean; message 'accessibilityIsIgnored';
    function accessibilityHitTest(point: NSPoint): id; message 'accessibilityHitTest:';
    function accessibilityFocusedUIElement: id; message 'accessibilityFocusedUIElement';
    function accessibilityIndexOfChild(child: id): NSUInteger; message 'accessibilityIndexOfChild:';
    function accessibilityArrayAttributeCount(attribute: NSString): NSUInteger; message 'accessibilityArrayAttributeCount:';
    function accessibilityArrayAttributeValues_index_maxCount(attribute: NSString; index: NSUInteger; maxCount: NSUInteger): NSArray; message 'accessibilityArrayAttributeValues:index:maxCount:';
  end;

{ NSAccessibilityAdditionsCategory }
  NSAccessibilityAdditionsCategory = objccategory external (NSObject)
    function accessibilitySetOverrideValue_forAttribute(value: id; attribute: NSString): Boolean; message 'accessibilitySetOverrideValue:forAttribute:';
  end;

{$endif}
{$endif}

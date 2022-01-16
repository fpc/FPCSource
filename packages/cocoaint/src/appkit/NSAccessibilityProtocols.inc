{ Parsed from AppKit.framework NSAccessibilityProtocols.h }


{$ifdef TYPES}
type
  NSAccessibilityElementProtocolPtr = ^NSAccessibilityElementProtocol;
  NSAccessibilityGroupProtocolPtr = ^NSAccessibilityGroupProtocol;
  NSAccessibilityButtonProtocolPtr = ^NSAccessibilityButtonProtocol;
  NSAccessibilitySwitchProtocolPtr = ^NSAccessibilitySwitchProtocol;
  NSAccessibilityRadioButtonProtocolPtr = ^NSAccessibilityRadioButtonProtocol;
  NSAccessibilityCheckBoxProtocolPtr = ^NSAccessibilityCheckBoxProtocol;
  NSAccessibilityStaticTextProtocolPtr = ^NSAccessibilityStaticTextProtocol;
  NSAccessibilityNavigableStaticTextProtocolPtr = ^NSAccessibilityNavigableStaticTextProtocol;
  NSAccessibilityProgressIndicatorProtocolPtr = ^NSAccessibilityProgressIndicatorProtocol;
  NSAccessibilityStepperProtocolPtr = ^NSAccessibilityStepperProtocol;
  NSAccessibilitySliderProtocolPtr = ^NSAccessibilitySliderProtocol;
  NSAccessibilityImageProtocolPtr = ^NSAccessibilityImageProtocol;
  NSAccessibilityContainsTransientUIProtocolPtr = ^NSAccessibilityContainsTransientUIProtocol;
  NSAccessibilityTableProtocolPtr = ^NSAccessibilityTableProtocol;
  NSAccessibilityOutlineProtocolPtr = ^NSAccessibilityOutlineProtocol;
  NSAccessibilityListProtocolPtr = ^NSAccessibilityListProtocol;
  NSAccessibilityRowProtocolPtr = ^NSAccessibilityRowProtocol;
  NSAccessibilityLayoutAreaProtocolPtr = ^NSAccessibilityLayoutAreaProtocol;
  NSAccessibilityLayoutItemProtocolPtr = ^NSAccessibilityLayoutItemProtocol;
  NSAccessibilityProtocolPtr = ^NSAccessibilityProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSAccessibilityElementProtocol = objcprotocol external name 'NSAccessibilityElement' (NSObjectProtocol)
  required
    function accessibilityFrame: NSRect; message 'accessibilityFrame';
    function accessibilityParent: id; message 'accessibilityParent';
  optional
    function isAccessibilityFocused: ObjCBOOL; message 'isAccessibilityFocused';
    function accessibilityIdentifier: NSString; message 'accessibilityIdentifier';
  end;

type
  NSAccessibilityGroupProtocol = objcprotocol external name 'NSAccessibilityGroup' (NSAccessibilityElementProtocol)
  end;

type
  NSAccessibilityButtonProtocol = objcprotocol external name 'NSAccessibilityButton' (NSAccessibilityElementProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    function accessibilityPerformPress: ObjCBOOL; message 'accessibilityPerformPress';
  end;

type
  NSAccessibilitySwitchProtocol = objcprotocol external name 'NSAccessibilitySwitch' (NSAccessibilityButtonProtocol)
  required
    function accessibilityValue: NSString; message 'accessibilityValue';
  optional
    function accessibilityPerformIncrement: ObjCBOOL; message 'accessibilityPerformIncrement';
    function accessibilityPerformDecrement: ObjCBOOL; message 'accessibilityPerformDecrement';
  end;

type
  NSAccessibilityRadioButtonProtocol = objcprotocol external name 'NSAccessibilityRadioButton' (NSAccessibilityButtonProtocol)
  required
    function accessibilityValue: NSNumber; message 'accessibilityValue';
  end;

type
  NSAccessibilityCheckBoxProtocol = objcprotocol external name 'NSAccessibilityCheckBox' (NSAccessibilityButtonProtocol)
  required
    function accessibilityValue: NSNumber; message 'accessibilityValue';
  end;

type
  NSAccessibilityStaticTextProtocol = objcprotocol external name 'NSAccessibilityStaticText' (NSAccessibilityElementProtocol)
  required
    function accessibilityValue: NSString; message 'accessibilityValue';
  optional
    function accessibilityAttributedStringForRange (range: NSRange): NSAttributedString; message 'accessibilityAttributedStringForRange:';
    function accessibilityVisibleCharacterRange: NSRange; message 'accessibilityVisibleCharacterRange';
  end;

type
  NSAccessibilityNavigableStaticTextProtocol = objcprotocol external name 'NSAccessibilityNavigableStaticText' (NSAccessibilityStaticTextProtocol)
  required
    function accessibilityStringForRange (range: NSRange): NSString; message 'accessibilityStringForRange:';
    function accessibilityLineForIndex (index: NSInteger): NSInteger; message 'accessibilityLineForIndex:';
    function accessibilityRangeForLine (lineNumber: NSInteger): NSRange; message 'accessibilityRangeForLine:';
    function accessibilityFrameForRange (range: NSRange): NSRect; message 'accessibilityFrameForRange:';
  end;

type
  NSAccessibilityProgressIndicatorProtocol = objcprotocol external name 'NSAccessibilityProgressIndicator' (NSAccessibilityGroupProtocol)
  required
    function accessibilityValue: NSNumber; message 'accessibilityValue';
  end;

type
  NSAccessibilityStepperProtocol = objcprotocol external name 'NSAccessibilityStepper' (NSAccessibilityElementProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    function accessibilityPerformIncrement: ObjCBOOL; message 'accessibilityPerformIncrement';
    function accessibilityPerformDecrement: ObjCBOOL; message 'accessibilityPerformDecrement';
  optional
    function accessibilityValue: id; message 'accessibilityValue';
  end;

type
  NSAccessibilitySliderProtocol = objcprotocol external name 'NSAccessibilitySlider' (NSAccessibilityElementProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    function accessibilityValue: id; message 'accessibilityValue';
    function accessibilityPerformIncrement: ObjCBOOL; message 'accessibilityPerformIncrement';
    function accessibilityPerformDecrement: ObjCBOOL; message 'accessibilityPerformDecrement';
  end;

type
  NSAccessibilityImageProtocol = objcprotocol external name 'NSAccessibilityImage' (NSAccessibilityElementProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
  end;

type
  NSAccessibilityContainsTransientUIProtocol = objcprotocol external name 'NSAccessibilityContainsTransientUI' (NSAccessibilityElementProtocol)
  required
    function accessibilityPerformShowAlternateUI: ObjCBOOL; message 'accessibilityPerformShowAlternateUI';
    function accessibilityPerformShowDefaultUI: ObjCBOOL; message 'accessibilityPerformShowDefaultUI';
    function isAccessibilityAlternateUIVisible: ObjCBOOL; message 'isAccessibilityAlternateUIVisible';
  end;

type
  NSAccessibilityTableProtocol = objcprotocol external name 'NSAccessibilityTable' (NSAccessibilityGroupProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    function accessibilityRows: NSArray; message 'accessibilityRows';
  optional
    function accessibilitySelectedRows: NSArray; message 'accessibilitySelectedRows';
    procedure setAccessibilitySelectedRows (selectedRows: NSArray); message 'setAccessibilitySelectedRows:';
    function accessibilityVisibleRows: NSArray; message 'accessibilityVisibleRows';
    function accessibilityColumns: NSArray; message 'accessibilityColumns';
    function accessibilityVisibleColumns: NSArray; message 'accessibilityVisibleColumns';
    function accessibilitySelectedColumns: NSArray; message 'accessibilitySelectedColumns';
    function accessibilityHeaderGroup: NSString; message 'accessibilityHeaderGroup';
    function accessibilitySelectedCells: NSArray; message 'accessibilitySelectedCells';
    function accessibilityVisibleCells: NSArray; message 'accessibilityVisibleCells';
    function accessibilityRowHeaderUIElements: NSArray; message 'accessibilityRowHeaderUIElements';
    function accessibilityColumnHeaderUIElements: NSArray; message 'accessibilityColumnHeaderUIElements';
  end;

type
  NSAccessibilityOutlineProtocol = objcprotocol external name 'NSAccessibilityOutline' (NSAccessibilityTableProtocol)
  end;

type
  NSAccessibilityListProtocol = objcprotocol external name 'NSAccessibilityList' (NSAccessibilityTableProtocol)
  end;

type
  NSAccessibilityRowProtocol = objcprotocol external name 'NSAccessibilityRow' (NSAccessibilityGroupProtocol)
  required
    function accessibilityIndex: NSInteger; message 'accessibilityIndex';
  optional
    function accessibilityDisclosureLevel: NSInteger; message 'accessibilityDisclosureLevel';
  end;

type
  NSAccessibilityLayoutAreaProtocol = objcprotocol external name 'NSAccessibilityLayoutArea' (NSAccessibilityGroupProtocol)
  required
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    function accessibilityChildren: NSArray; message 'accessibilityChildren';
    function accessibilitySelectedChildren: NSArray; message 'accessibilitySelectedChildren';
    function accessibilityFocusedUIElement: id; message 'accessibilityFocusedUIElement';
  end;

type
  NSAccessibilityLayoutItemProtocol = objcprotocol external name 'NSAccessibilityLayoutItem' (NSAccessibilityGroupProtocol)
  optional
    procedure setAccessibilityFrame (frame: NSRect); message 'setAccessibilityFrame:';
  end;

type
  NSAccessibilityProtocol = objcprotocol external name 'NSAccessibility' (NSObjectProtocol)
  required
    procedure setAccessibilityElement(newValue: ObjCBOOL); message 'setAccessibilityElement:';
    function isAccessibilityElement: ObjCBOOL; message 'isAccessibilityElement';
    procedure setAccessibilityFrame(newValue: NSRect); message 'setAccessibilityFrame:';
    function accessibilityFrame: NSRect; message 'accessibilityFrame';
    procedure setAccessibilityFocused(newValue: ObjCBOOL); message 'setAccessibilityFocused:';
    function isAccessibilityFocused: ObjCBOOL; message 'isAccessibilityFocused';
    procedure setAccessibilityActivationPoint(newValue: NSPoint); message 'setAccessibilityActivationPoint:';
    function accessibilityActivationPoint: NSPoint; message 'accessibilityActivationPoint';
    procedure setAccessibilityTopLevelUIElement(newValue: id); message 'setAccessibilityTopLevelUIElement:';
    function accessibilityTopLevelUIElement: id; message 'accessibilityTopLevelUIElement';
    procedure setAccessibilityURL(newValue: NSURL); message 'setAccessibilityURL:';
    function accessibilityURL: NSURL; message 'accessibilityURL';
    procedure setAccessibilityValue(newValue: id); message 'setAccessibilityValue:';
    function accessibilityValue: id; message 'accessibilityValue';
    procedure setAccessibilityValueDescription(newValue: NSString); message 'setAccessibilityValueDescription:';
    function accessibilityValueDescription: NSString; message 'accessibilityValueDescription';
    procedure setAccessibilityVisibleChildren(newValue: NSArray); message 'setAccessibilityVisibleChildren:';
    function accessibilityVisibleChildren: NSArray; message 'accessibilityVisibleChildren';
    procedure setAccessibilitySubrole(newValue: NSString); message 'setAccessibilitySubrole:';
    function accessibilitySubrole: NSString; message 'accessibilitySubrole';
    procedure setAccessibilityTitle(newValue: NSString); message 'setAccessibilityTitle:';
    function accessibilityTitle: NSString; message 'accessibilityTitle';
    procedure setAccessibilityTitleUIElement(newValue: id); message 'setAccessibilityTitleUIElement:';
    function accessibilityTitleUIElement: id; message 'accessibilityTitleUIElement';
    procedure setAccessibilityNextContents(newValue: NSArray); message 'setAccessibilityNextContents:';
    function accessibilityNextContents: NSArray; message 'accessibilityNextContents';
    procedure setAccessibilityOrientation(newValue: NSAccessibilityOrientation); message 'setAccessibilityOrientation:';
    function accessibilityOrientation: NSAccessibilityOrientation; message 'accessibilityOrientation';
    procedure setAccessibilityOverflowButton(newValue: id); message 'setAccessibilityOverflowButton:';
    function accessibilityOverflowButton: id; message 'accessibilityOverflowButton';
    procedure setAccessibilityParent(newValue: id); message 'setAccessibilityParent:';
    function accessibilityParent: id; message 'accessibilityParent';
    procedure setAccessibilityPlaceholderValue(newValue: NSString); message 'setAccessibilityPlaceholderValue:';
    function accessibilityPlaceholderValue: NSString; message 'accessibilityPlaceholderValue';
    procedure setAccessibilityPreviousContents(newValue: NSArray); message 'setAccessibilityPreviousContents:';
    function accessibilityPreviousContents: NSArray; message 'accessibilityPreviousContents';
    procedure setAccessibilityRole(newValue: NSString); message 'setAccessibilityRole:';
    function accessibilityRole: NSString; message 'accessibilityRole';
    procedure setAccessibilityRoleDescription(newValue: NSString); message 'setAccessibilityRoleDescription:';
    function accessibilityRoleDescription: NSString; message 'accessibilityRoleDescription';
    procedure setAccessibilitySearchButton(newValue: id); message 'setAccessibilitySearchButton:';
    function accessibilitySearchButton: id; message 'accessibilitySearchButton';
    procedure setAccessibilitySearchMenu(newValue: id); message 'setAccessibilitySearchMenu:';
    function accessibilitySearchMenu: id; message 'accessibilitySearchMenu';
    procedure setAccessibilitySelected(newValue: ObjCBOOL); message 'setAccessibilitySelected:';
    function isAccessibilitySelected: ObjCBOOL; message 'isAccessibilitySelected';
    procedure setAccessibilitySelectedChildren(newValue: NSArray); message 'setAccessibilitySelectedChildren:';
    function accessibilitySelectedChildren: NSArray; message 'accessibilitySelectedChildren';
    procedure setAccessibilityServesAsTitleForUIElements(newValue: NSArray); message 'setAccessibilityServesAsTitleForUIElements:';
    function accessibilityServesAsTitleForUIElements: NSArray; message 'accessibilityServesAsTitleForUIElements';
    procedure setAccessibilityShownMenu(newValue: id); message 'setAccessibilityShownMenu:';
    function accessibilityShownMenu: id; message 'accessibilityShownMenu';
    procedure setAccessibilityMinValue(newValue: id); message 'setAccessibilityMinValue:';
    function accessibilityMinValue: id; message 'accessibilityMinValue';
    procedure setAccessibilityMaxValue(newValue: id); message 'setAccessibilityMaxValue:';
    function accessibilityMaxValue: id; message 'accessibilityMaxValue';
    procedure setAccessibilityLinkedUIElements(newValue: NSArray); message 'setAccessibilityLinkedUIElements:';
    function accessibilityLinkedUIElements: NSArray; message 'accessibilityLinkedUIElements';
    procedure setAccessibilityWindow(newValue: id); message 'setAccessibilityWindow:';
    function accessibilityWindow: id; message 'accessibilityWindow';
    procedure setAccessibilityIdentifier(newValue: NSString); message 'setAccessibilityIdentifier:';
    function accessibilityIdentifier: NSString; message 'accessibilityIdentifier';
    procedure setAccessibilityHelp(newValue: NSString); message 'setAccessibilityHelp:';
    function accessibilityHelp: NSString; message 'accessibilityHelp';
    procedure setAccessibilityFilename(newValue: NSString); message 'setAccessibilityFilename:';
    function accessibilityFilename: NSString; message 'accessibilityFilename';
    procedure setAccessibilityExpanded(newValue: ObjCBOOL); message 'setAccessibilityExpanded:';
    function isAccessibilityExpanded: ObjCBOOL; message 'isAccessibilityExpanded';
    procedure setAccessibilityEdited(newValue: ObjCBOOL); message 'setAccessibilityEdited:';
    function isAccessibilityEdited: ObjCBOOL; message 'isAccessibilityEdited';
    procedure setAccessibilityEnabled(newValue: ObjCBOOL); message 'setAccessibilityEnabled:';
    function isAccessibilityEnabled: ObjCBOOL; message 'isAccessibilityEnabled';
    procedure setAccessibilityChildren(newValue: NSArray); message 'setAccessibilityChildren:';
    function accessibilityChildren: NSArray; message 'accessibilityChildren';
    procedure setAccessibilityClearButton(newValue: id); message 'setAccessibilityClearButton:';
    function accessibilityClearButton: id; message 'accessibilityClearButton';
    procedure setAccessibilityCancelButton(newValue: id); message 'setAccessibilityCancelButton:';
    function accessibilityCancelButton: id; message 'accessibilityCancelButton';
    procedure setAccessibilityProtectedContent(newValue: ObjCBOOL); message 'setAccessibilityProtectedContent:';
    function isAccessibilityProtectedContent: ObjCBOOL; message 'isAccessibilityProtectedContent';
    procedure setAccessibilityContents(newValue: NSArray); message 'setAccessibilityContents:';
    function accessibilityContents: NSArray; message 'accessibilityContents';
    procedure setAccessibilityLabel(newValue: NSString); message 'setAccessibilityLabel:';
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    procedure setAccessibilityAlternateUIVisible(newValue: ObjCBOOL); message 'setAccessibilityAlternateUIVisible:';
    function isAccessibilityAlternateUIVisible: ObjCBOOL; message 'isAccessibilityAlternateUIVisible';
    procedure setAccessibilitySharedFocusElements(newValue: NSArray); message 'setAccessibilitySharedFocusElements:';
    function accessibilitySharedFocusElements: NSArray; message 'accessibilitySharedFocusElements';
    procedure setAccessibilityApplicationFocusedUIElement(newValue: id); message 'setAccessibilityApplicationFocusedUIElement:';
    function accessibilityApplicationFocusedUIElement: id; message 'accessibilityApplicationFocusedUIElement';
    procedure setAccessibilityMainWindow(newValue: id); message 'setAccessibilityMainWindow:';
    function accessibilityMainWindow: id; message 'accessibilityMainWindow';
    procedure setAccessibilityHidden(newValue: ObjCBOOL); message 'setAccessibilityHidden:';
    function isAccessibilityHidden: ObjCBOOL; message 'isAccessibilityHidden';
    procedure setAccessibilityFrontmost(newValue: ObjCBOOL); message 'setAccessibilityFrontmost:';
    function isAccessibilityFrontmost: ObjCBOOL; message 'isAccessibilityFrontmost';
    procedure setAccessibilityFocusedWindow(newValue: id); message 'setAccessibilityFocusedWindow:';
    function accessibilityFocusedWindow: id; message 'accessibilityFocusedWindow';
    procedure setAccessibilityWindows(newValue: NSArray); message 'setAccessibilityWindows:';
    function accessibilityWindows: NSArray; message 'accessibilityWindows';
    procedure setAccessibilityExtrasMenuBar(newValue: id); message 'setAccessibilityExtrasMenuBar:';
    function accessibilityExtrasMenuBar: id; message 'accessibilityExtrasMenuBar';
    procedure setAccessibilityMenuBar(newValue: id); message 'setAccessibilityMenuBar:';
    function accessibilityMenuBar: id; message 'accessibilityMenuBar';
    procedure setAccessibilityColumnTitles(newValue: NSArray); message 'setAccessibilityColumnTitles:';
    function accessibilityColumnTitles: NSArray; message 'accessibilityColumnTitles';
    procedure setAccessibilityOrderedByRow(newValue: ObjCBOOL); message 'setAccessibilityOrderedByRow:';
    function isAccessibilityOrderedByRow: ObjCBOOL; message 'isAccessibilityOrderedByRow';
    procedure setAccessibilityHorizontalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityHorizontalUnits:';
    function accessibilityHorizontalUnits: NSAccessibilityUnits; message 'accessibilityHorizontalUnits';
    procedure setAccessibilityVerticalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityVerticalUnits:';
    function accessibilityVerticalUnits: NSAccessibilityUnits; message 'accessibilityVerticalUnits';
    procedure setAccessibilityHorizontalUnitDescription(newValue: NSString); message 'setAccessibilityHorizontalUnitDescription:';
    function accessibilityHorizontalUnitDescription: NSString; message 'accessibilityHorizontalUnitDescription';
    procedure setAccessibilityVerticalUnitDescription(newValue: NSString); message 'setAccessibilityVerticalUnitDescription:';
    function accessibilityVerticalUnitDescription: NSString; message 'accessibilityVerticalUnitDescription';
    function accessibilityLayoutPointForScreenPoint (point: NSPoint): NSPoint; message 'accessibilityLayoutPointForScreenPoint:'; { available in 10_10 }
    function accessibilityLayoutSizeForScreenSize (size: NSSize): NSSize; message 'accessibilityLayoutSizeForScreenSize:'; { available in 10_10 }
    function accessibilityScreenPointForLayoutPoint (point: NSPoint): NSPoint; message 'accessibilityScreenPointForLayoutPoint:'; { available in 10_10 }
    function accessibilityScreenSizeForLayoutSize (size: NSSize): NSSize; message 'accessibilityScreenSizeForLayoutSize:'; { available in 10_10 }
    procedure setAccessibilityHandles(newValue: NSArray); message 'setAccessibilityHandles:';
    function accessibilityHandles: NSArray; message 'accessibilityHandles';
    procedure setAccessibilityWarningValue(newValue: id); message 'setAccessibilityWarningValue:';
    function accessibilityWarningValue: id; message 'accessibilityWarningValue';
    procedure setAccessibilityCriticalValue(newValue: id); message 'setAccessibilityCriticalValue:';
    function accessibilityCriticalValue: id; message 'accessibilityCriticalValue';
    procedure setAccessibilityDisclosed(newValue: ObjCBOOL); message 'setAccessibilityDisclosed:';
    function isAccessibilityDisclosed: ObjCBOOL; message 'isAccessibilityDisclosed';
    procedure setAccessibilityDisclosedByRow(newValue: id); message 'setAccessibilityDisclosedByRow:';
    function accessibilityDisclosedByRow: id; message 'accessibilityDisclosedByRow';
    procedure setAccessibilityDisclosedRows(newValue: id); message 'setAccessibilityDisclosedRows:';
    function accessibilityDisclosedRows: id; message 'accessibilityDisclosedRows';
    procedure setAccessibilityDisclosureLevel(newValue: NSInteger); message 'setAccessibilityDisclosureLevel:';
    function accessibilityDisclosureLevel: NSInteger; message 'accessibilityDisclosureLevel';
    procedure setAccessibilityMarkerUIElements(newValue: NSArray); message 'setAccessibilityMarkerUIElements:';
    function accessibilityMarkerUIElements: NSArray; message 'accessibilityMarkerUIElements';
    procedure setAccessibilityMarkerValues(newValue: id); message 'setAccessibilityMarkerValues:';
    function accessibilityMarkerValues: id; message 'accessibilityMarkerValues';
    procedure setAccessibilityMarkerGroupUIElement(newValue: id); message 'setAccessibilityMarkerGroupUIElement:';
    function accessibilityMarkerGroupUIElement: id; message 'accessibilityMarkerGroupUIElement';
    procedure setAccessibilityUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityUnits:';
    function accessibilityUnits: NSAccessibilityUnits; message 'accessibilityUnits';
    procedure setAccessibilityUnitDescription(newValue: NSString); message 'setAccessibilityUnitDescription:';
    function accessibilityUnitDescription: NSString; message 'accessibilityUnitDescription';
    procedure setAccessibilityRulerMarkerType(newValue: NSAccessibilityRulerMarkerType); message 'setAccessibilityRulerMarkerType:';
    function accessibilityRulerMarkerType: NSAccessibilityRulerMarkerType; message 'accessibilityRulerMarkerType';
    procedure setAccessibilityMarkerTypeDescription(newValue: NSString); message 'setAccessibilityMarkerTypeDescription:';
    function accessibilityMarkerTypeDescription: NSString; message 'accessibilityMarkerTypeDescription';
    procedure setAccessibilityHorizontalScrollBar(newValue: id); message 'setAccessibilityHorizontalScrollBar:';
    function accessibilityHorizontalScrollBar: id; message 'accessibilityHorizontalScrollBar';
    procedure setAccessibilityVerticalScrollBar(newValue: id); message 'setAccessibilityVerticalScrollBar:';
    function accessibilityVerticalScrollBar: id; message 'accessibilityVerticalScrollBar';
    procedure setAccessibilityAllowedValues(newValue: NSArray); message 'setAccessibilityAllowedValues:';
    function accessibilityAllowedValues: NSArray; message 'accessibilityAllowedValues';
    procedure setAccessibilityLabelUIElements(newValue: NSArray); message 'setAccessibilityLabelUIElements:';
    function accessibilityLabelUIElements: NSArray; message 'accessibilityLabelUIElements';
    procedure setAccessibilityLabelValue(newValue: single); message 'setAccessibilityLabelValue:';
    function accessibilityLabelValue: single; message 'accessibilityLabelValue';
    procedure setAccessibilitySplitters(newValue: NSArray); message 'setAccessibilitySplitters:';
    function accessibilitySplitters: NSArray; message 'accessibilitySplitters';
    procedure setAccessibilityDecrementButton(newValue: id); message 'setAccessibilityDecrementButton:';
    function accessibilityDecrementButton: id; message 'accessibilityDecrementButton';
    procedure setAccessibilityIncrementButton(newValue: id); message 'setAccessibilityIncrementButton:';
    function accessibilityIncrementButton: id; message 'accessibilityIncrementButton';
    procedure setAccessibilityTabs(newValue: NSArray); message 'setAccessibilityTabs:';
    function accessibilityTabs: NSArray; message 'accessibilityTabs';
    procedure setAccessibilityHeader(newValue: id); message 'setAccessibilityHeader:';
    function accessibilityHeader: id; message 'accessibilityHeader';
    procedure setAccessibilityColumnCount(newValue: NSInteger); message 'setAccessibilityColumnCount:';
    function accessibilityColumnCount: NSInteger; message 'accessibilityColumnCount';
    procedure setAccessibilityRowCount(newValue: NSInteger); message 'setAccessibilityRowCount:';
    function accessibilityRowCount: NSInteger; message 'accessibilityRowCount';
    procedure setAccessibilityIndex(newValue: NSInteger); message 'setAccessibilityIndex:';
    function accessibilityIndex: NSInteger; message 'accessibilityIndex';
    procedure setAccessibilityColumns(newValue: NSArray); message 'setAccessibilityColumns:';
    function accessibilityColumns: NSArray; message 'accessibilityColumns';
    procedure setAccessibilityRows(newValue: NSArray); message 'setAccessibilityRows:';
    function accessibilityRows: NSArray; message 'accessibilityRows';
    procedure setAccessibilityVisibleRows(newValue: NSArray); message 'setAccessibilityVisibleRows:';
    function accessibilityVisibleRows: NSArray; message 'accessibilityVisibleRows';
    procedure setAccessibilitySelectedRows(newValue: NSArray); message 'setAccessibilitySelectedRows:';
    function accessibilitySelectedRows: NSArray; message 'accessibilitySelectedRows';
    procedure setAccessibilityVisibleColumns(newValue: NSArray); message 'setAccessibilityVisibleColumns:';
    function accessibilityVisibleColumns: NSArray; message 'accessibilityVisibleColumns';
    procedure setAccessibilitySelectedColumns(newValue: NSArray); message 'setAccessibilitySelectedColumns:';
    function accessibilitySelectedColumns: NSArray; message 'accessibilitySelectedColumns';
    procedure setAccessibilitySortDirection(newValue: NSAccessibilitySortDirection); message 'setAccessibilitySortDirection:';
    function accessibilitySortDirection: NSAccessibilitySortDirection; message 'accessibilitySortDirection';
    procedure setAccessibilityRowHeaderUIElements(newValue: NSArray); message 'setAccessibilityRowHeaderUIElements:';
    function accessibilityRowHeaderUIElements: NSArray; message 'accessibilityRowHeaderUIElements';
    procedure setAccessibilitySelectedCells(newValue: NSArray); message 'setAccessibilitySelectedCells:';
    function accessibilitySelectedCells: NSArray; message 'accessibilitySelectedCells';
    procedure setAccessibilityVisibleCells(newValue: NSArray); message 'setAccessibilityVisibleCells:';
    function accessibilityVisibleCells: NSArray; message 'accessibilityVisibleCells';
    procedure setAccessibilityColumnHeaderUIElements(newValue: NSArray); message 'setAccessibilityColumnHeaderUIElements:';
    function accessibilityColumnHeaderUIElements: NSArray; message 'accessibilityColumnHeaderUIElements';
    function accessibilityCellForColumn_row (column: NSInteger; row: NSInteger): id; message 'accessibilityCellForColumn:row:'; { available in 10_10 }
    procedure setAccessibilityRowIndexRange(newValue: NSRange); message 'setAccessibilityRowIndexRange:';
    function accessibilityRowIndexRange: NSRange; message 'accessibilityRowIndexRange';
    procedure setAccessibilityColumnIndexRange(newValue: NSRange); message 'setAccessibilityColumnIndexRange:';
    function accessibilityColumnIndexRange: NSRange; message 'accessibilityColumnIndexRange';
    procedure setAccessibilityInsertionPointLineNumber(newValue: NSInteger); message 'setAccessibilityInsertionPointLineNumber:';
    function accessibilityInsertionPointLineNumber: NSInteger; message 'accessibilityInsertionPointLineNumber';
    procedure setAccessibilitySharedCharacterRange(newValue: NSRange); message 'setAccessibilitySharedCharacterRange:';
    function accessibilitySharedCharacterRange: NSRange; message 'accessibilitySharedCharacterRange';
    procedure setAccessibilitySharedTextUIElements(newValue: NSArray); message 'setAccessibilitySharedTextUIElements:';
    function accessibilitySharedTextUIElements: NSArray; message 'accessibilitySharedTextUIElements';
    procedure setAccessibilityVisibleCharacterRange(newValue: NSRange); message 'setAccessibilityVisibleCharacterRange:';
    function accessibilityVisibleCharacterRange: NSRange; message 'accessibilityVisibleCharacterRange';
    procedure setAccessibilityNumberOfCharacters(newValue: NSInteger); message 'setAccessibilityNumberOfCharacters:';
    function accessibilityNumberOfCharacters: NSInteger; message 'accessibilityNumberOfCharacters';
    procedure setAccessibilitySelectedText(newValue: NSString); message 'setAccessibilitySelectedText:';
    function accessibilitySelectedText: NSString; message 'accessibilitySelectedText';
    procedure setAccessibilitySelectedTextRange(newValue: NSRange); message 'setAccessibilitySelectedTextRange:';
    function accessibilitySelectedTextRange: NSRange; message 'accessibilitySelectedTextRange';
    procedure setAccessibilitySelectedTextRanges(newValue: NSArray); message 'setAccessibilitySelectedTextRanges:';
    function accessibilitySelectedTextRanges: NSArray; message 'accessibilitySelectedTextRanges';
    function accessibilityAttributedStringForRange (range: NSRange): NSAttributedString; message 'accessibilityAttributedStringForRange:'; { available in 10_10 }
    function accessibilityRangeForLine (line: NSInteger): NSRange; message 'accessibilityRangeForLine:'; { available in 10_10 }
    function accessibilityStringForRange (range: NSRange): NSString; message 'accessibilityStringForRange:'; { available in 10_10 }
    function accessibilityRangeForPosition (point: NSPoint): NSRange; message 'accessibilityRangeForPosition:'; { available in 10_10 }
    function accessibilityRangeForIndex (index: NSInteger): NSRange; message 'accessibilityRangeForIndex:'; { available in 10_10 }
    function accessibilityFrameForRange (range: NSRange): NSRect; message 'accessibilityFrameForRange:'; { available in 10_10 }
    function accessibilityRTFForRange (range: NSRange): NSData; message 'accessibilityRTFForRange:'; { available in 10_10 }
    function accessibilityStyleRangeForIndex (index: NSInteger): NSRange; message 'accessibilityStyleRangeForIndex:'; { available in 10_10 }
    function accessibilityLineForIndex (index: NSInteger): NSInteger; message 'accessibilityLineForIndex:'; { available in 10_10 }
    procedure setAccessibilityToolbarButton(newValue: id); message 'setAccessibilityToolbarButton:';
    function accessibilityToolbarButton: id; message 'accessibilityToolbarButton';
    procedure setAccessibilityModal(newValue: ObjCBOOL); message 'setAccessibilityModal:';
    function isAccessibilityModal: ObjCBOOL; message 'isAccessibilityModal';
    procedure setAccessibilityProxy(newValue: id); message 'setAccessibilityProxy:';
    function accessibilityProxy: id; message 'accessibilityProxy';
    procedure setAccessibilityMain(newValue: ObjCBOOL); message 'setAccessibilityMain:';
    function isAccessibilityMain: ObjCBOOL; message 'isAccessibilityMain';
    procedure setAccessibilityFullScreenButton(newValue: id); message 'setAccessibilityFullScreenButton:';
    function accessibilityFullScreenButton: id; message 'accessibilityFullScreenButton';
    procedure setAccessibilityGrowArea(newValue: id); message 'setAccessibilityGrowArea:';
    function accessibilityGrowArea: id; message 'accessibilityGrowArea';
    procedure setAccessibilityDocument(newValue: NSString); message 'setAccessibilityDocument:';
    function accessibilityDocument: NSString; message 'accessibilityDocument';
    procedure setAccessibilityDefaultButton(newValue: id); message 'setAccessibilityDefaultButton:';
    function accessibilityDefaultButton: id; message 'accessibilityDefaultButton';
    procedure setAccessibilityCloseButton(newValue: id); message 'setAccessibilityCloseButton:';
    function accessibilityCloseButton: id; message 'accessibilityCloseButton';
    procedure setAccessibilityZoomButton(newValue: id); message 'setAccessibilityZoomButton:';
    function accessibilityZoomButton: id; message 'accessibilityZoomButton';
    procedure setAccessibilityMinimizeButton(newValue: id); message 'setAccessibilityMinimizeButton:';
    function accessibilityMinimizeButton: id; message 'accessibilityMinimizeButton';
    procedure setAccessibilityMinimized(newValue: ObjCBOOL); message 'setAccessibilityMinimized:';
    function isAccessibilityMinimized: ObjCBOOL; message 'isAccessibilityMinimized';
    function accessibilityPerformCancel: ObjCBOOL; message 'accessibilityPerformCancel'; { available in 10_10 }
    function accessibilityPerformConfirm: ObjCBOOL; message 'accessibilityPerformConfirm'; { available in 10_10 }
    function accessibilityPerformDecrement: ObjCBOOL; message 'accessibilityPerformDecrement'; { available in 10_10 }
    function accessibilityPerformDelete: ObjCBOOL; message 'accessibilityPerformDelete'; { available in 10_10 }
    function accessibilityPerformIncrement: ObjCBOOL; message 'accessibilityPerformIncrement'; { available in 10_10 }
    function accessibilityPerformPick: ObjCBOOL; message 'accessibilityPerformPick'; { available in 10_10 }
    function accessibilityPerformPress: ObjCBOOL; message 'accessibilityPerformPress'; { available in 10_10 }
    function accessibilityPerformRaise: ObjCBOOL; message 'accessibilityPerformRaise'; { available in 10_10 }
    function accessibilityPerformShowAlternateUI: ObjCBOOL; message 'accessibilityPerformShowAlternateUI'; { available in 10_10 }
    function accessibilityPerformShowDefaultUI: ObjCBOOL; message 'accessibilityPerformShowDefaultUI'; { available in 10_10 }
    function accessibilityPerformShowMenu: ObjCBOOL; message 'accessibilityPerformShowMenu'; { available in 10_10 }
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
  end;
{$endif}


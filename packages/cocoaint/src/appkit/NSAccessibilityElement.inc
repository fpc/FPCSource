{ Parsed from AppKit.framework NSAccessibilityElement.h }


{$ifdef TYPES}
type
  NSAccessibilityElementPtr = ^NSAccessibilityElement;
{$endif}

{$ifdef CLASSES}

type
  NSAccessibilityElement = objcclass external (NSObject, NSAccessibilityProtocol)
  private
    _private: id;
  public
    class function accessibilityElementWithRole_frame_label_parent (role: NSString; frame: NSRect; label_: NSString; parent: id): id; message 'accessibilityElementWithRole:frame:label:parent:';
    procedure accessibilityAddChildElement (childElement: NSAccessibilityElement); message 'accessibilityAddChildElement:';
    procedure setAccessibilityFrameInParentSpace(newValue: NSRect); message 'setAccessibilityFrameInParentSpace:';
    function accessibilityFrameInParentSpace: NSRect; message 'accessibilityFrameInParentSpace';

    { Adopted protocols }
    procedure setAccessibilityActivationPoint(newValue: NSPoint); message 'setAccessibilityActivationPoint:';
    function accessibilityActivationPoint: NSPoint; message 'accessibilityActivationPoint';
    procedure setAccessibilityAllowedValues(newValue: NSArray); message 'setAccessibilityAllowedValues:';
    function accessibilityAllowedValues: NSArray; message 'accessibilityAllowedValues';
    procedure setAccessibilityAlternateUIVisible(newValue: ObjCBOOL); message 'setAccessibilityAlternateUIVisible:';
    function isAccessibilityAlternateUIVisible: ObjCBOOL; message 'isAccessibilityAlternateUIVisible';
    procedure setAccessibilityApplicationFocusedUIElement(newValue: id); message 'setAccessibilityApplicationFocusedUIElement:';
    function accessibilityApplicationFocusedUIElement: id; message 'accessibilityApplicationFocusedUIElement';
    function accessibilityAttributedStringForRange (range: NSRange): NSAttributedString; message 'accessibilityAttributedStringForRange:'; { available in 10_10 }
    procedure setAccessibilityCancelButton(newValue: id); message 'setAccessibilityCancelButton:';
    function accessibilityCancelButton: id; message 'accessibilityCancelButton';
    function accessibilityCellForColumn_row (column: NSInteger; row: NSInteger): id; message 'accessibilityCellForColumn:row:'; { available in 10_10 }
    procedure setAccessibilityChildren(newValue: NSArray); message 'setAccessibilityChildren:';
    function accessibilityChildren: NSArray; message 'accessibilityChildren';
    procedure setAccessibilityClearButton(newValue: id); message 'setAccessibilityClearButton:';
    function accessibilityClearButton: id; message 'accessibilityClearButton';
    procedure setAccessibilityCloseButton(newValue: id); message 'setAccessibilityCloseButton:';
    function accessibilityCloseButton: id; message 'accessibilityCloseButton';
    procedure setAccessibilityColumnCount(newValue: NSInteger); message 'setAccessibilityColumnCount:';
    function accessibilityColumnCount: NSInteger; message 'accessibilityColumnCount';
    procedure setAccessibilityColumnHeaderUIElements(newValue: NSArray); message 'setAccessibilityColumnHeaderUIElements:';
    function accessibilityColumnHeaderUIElements: NSArray; message 'accessibilityColumnHeaderUIElements';
    procedure setAccessibilityColumnIndexRange(newValue: NSRange); message 'setAccessibilityColumnIndexRange:';
    function accessibilityColumnIndexRange: NSRange; message 'accessibilityColumnIndexRange';
    procedure setAccessibilityColumns(newValue: NSArray); message 'setAccessibilityColumns:';
    function accessibilityColumns: NSArray; message 'accessibilityColumns';
    procedure setAccessibilityColumnTitles(newValue: NSArray); message 'setAccessibilityColumnTitles:';
    function accessibilityColumnTitles: NSArray; message 'accessibilityColumnTitles';
    procedure setAccessibilityContents(newValue: NSArray); message 'setAccessibilityContents:';
    function accessibilityContents: NSArray; message 'accessibilityContents';
    procedure setAccessibilityCriticalValue(newValue: id); message 'setAccessibilityCriticalValue:';
    function accessibilityCriticalValue: id; message 'accessibilityCriticalValue';
    procedure setAccessibilityDecrementButton(newValue: id); message 'setAccessibilityDecrementButton:';
    function accessibilityDecrementButton: id; message 'accessibilityDecrementButton';
    procedure setAccessibilityDefaultButton(newValue: id); message 'setAccessibilityDefaultButton:';
    function accessibilityDefaultButton: id; message 'accessibilityDefaultButton';
    procedure setAccessibilityDisclosed(newValue: ObjCBOOL); message 'setAccessibilityDisclosed:';
    function isAccessibilityDisclosed: ObjCBOOL; message 'isAccessibilityDisclosed';
    procedure setAccessibilityDisclosedByRow(newValue: id); message 'setAccessibilityDisclosedByRow:';
    function accessibilityDisclosedByRow: id; message 'accessibilityDisclosedByRow';
    procedure setAccessibilityDisclosedRows(newValue: id); message 'setAccessibilityDisclosedRows:';
    function accessibilityDisclosedRows: id; message 'accessibilityDisclosedRows';
    procedure setAccessibilityDisclosureLevel(newValue: NSInteger); message 'setAccessibilityDisclosureLevel:';
    function accessibilityDisclosureLevel: NSInteger; message 'accessibilityDisclosureLevel';
    procedure setAccessibilityDocument(newValue: NSString); message 'setAccessibilityDocument:';
    function accessibilityDocument: NSString; message 'accessibilityDocument';
    procedure setAccessibilityEdited(newValue: ObjCBOOL); message 'setAccessibilityEdited:';
    function isAccessibilityEdited: ObjCBOOL; message 'isAccessibilityEdited';
    procedure setAccessibilityElement(newValue: ObjCBOOL); message 'setAccessibilityElement:';
    function isAccessibilityElement: ObjCBOOL; message 'isAccessibilityElement';
    procedure setAccessibilityEnabled(newValue: ObjCBOOL); message 'setAccessibilityEnabled:';
    function isAccessibilityEnabled: ObjCBOOL; message 'isAccessibilityEnabled';
    procedure setAccessibilityExpanded(newValue: ObjCBOOL); message 'setAccessibilityExpanded:';
    function isAccessibilityExpanded: ObjCBOOL; message 'isAccessibilityExpanded';
    procedure setAccessibilityExtrasMenuBar(newValue: id); message 'setAccessibilityExtrasMenuBar:';
    function accessibilityExtrasMenuBar: id; message 'accessibilityExtrasMenuBar';
    procedure setAccessibilityFilename(newValue: NSString); message 'setAccessibilityFilename:';
    function accessibilityFilename: NSString; message 'accessibilityFilename';
    procedure setAccessibilityFocused(newValue: ObjCBOOL); message 'setAccessibilityFocused:';
    function isAccessibilityFocused: ObjCBOOL; message 'isAccessibilityFocused';
    procedure setAccessibilityFocusedWindow(newValue: id); message 'setAccessibilityFocusedWindow:';
    function accessibilityFocusedWindow: id; message 'accessibilityFocusedWindow';
    procedure setAccessibilityFrame(newValue: NSRect); message 'setAccessibilityFrame:';
    function accessibilityFrame: NSRect; message 'accessibilityFrame';
    function accessibilityFrameForRange (range: NSRange): NSRect; message 'accessibilityFrameForRange:'; { available in 10_10 }
    procedure setAccessibilityFrontmost(newValue: ObjCBOOL); message 'setAccessibilityFrontmost:';
    function isAccessibilityFrontmost: ObjCBOOL; message 'isAccessibilityFrontmost';
    procedure setAccessibilityFullScreenButton(newValue: id); message 'setAccessibilityFullScreenButton:';
    function accessibilityFullScreenButton: id; message 'accessibilityFullScreenButton';
    procedure setAccessibilityGrowArea(newValue: id); message 'setAccessibilityGrowArea:';
    function accessibilityGrowArea: id; message 'accessibilityGrowArea';
    procedure setAccessibilityHandles(newValue: NSArray); message 'setAccessibilityHandles:';
    function accessibilityHandles: NSArray; message 'accessibilityHandles';
    procedure setAccessibilityHeader(newValue: id); message 'setAccessibilityHeader:';
    function accessibilityHeader: id; message 'accessibilityHeader';
    procedure setAccessibilityHelp(newValue: NSString); message 'setAccessibilityHelp:';
    function accessibilityHelp: NSString; message 'accessibilityHelp';
    procedure setAccessibilityHidden(newValue: ObjCBOOL); message 'setAccessibilityHidden:';
    function isAccessibilityHidden: ObjCBOOL; message 'isAccessibilityHidden';
    procedure setAccessibilityHorizontalScrollBar(newValue: id); message 'setAccessibilityHorizontalScrollBar:';
    function accessibilityHorizontalScrollBar: id; message 'accessibilityHorizontalScrollBar';
    procedure setAccessibilityHorizontalUnitDescription(newValue: NSString); message 'setAccessibilityHorizontalUnitDescription:';
    function accessibilityHorizontalUnitDescription: NSString; message 'accessibilityHorizontalUnitDescription';
    procedure setAccessibilityHorizontalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityHorizontalUnits:';
    function accessibilityHorizontalUnits: NSAccessibilityUnits; message 'accessibilityHorizontalUnits';
    procedure setAccessibilityIdentifier(newValue: NSString); message 'setAccessibilityIdentifier:';
    function accessibilityIdentifier: NSString; message 'accessibilityIdentifier';
    procedure setAccessibilityIncrementButton(newValue: id); message 'setAccessibilityIncrementButton:';
    function accessibilityIncrementButton: id; message 'accessibilityIncrementButton';
    procedure setAccessibilityIndex(newValue: NSInteger); message 'setAccessibilityIndex:';
    function accessibilityIndex: NSInteger; message 'accessibilityIndex';
    procedure setAccessibilityInsertionPointLineNumber(newValue: NSInteger); message 'setAccessibilityInsertionPointLineNumber:';
    function accessibilityInsertionPointLineNumber: NSInteger; message 'accessibilityInsertionPointLineNumber';
    procedure setAccessibilityLabel(newValue: NSString); message 'setAccessibilityLabel:';
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    procedure setAccessibilityLabelUIElements(newValue: NSArray); message 'setAccessibilityLabelUIElements:';
    function accessibilityLabelUIElements: NSArray; message 'accessibilityLabelUIElements';
    procedure setAccessibilityLabelValue(newValue: single); message 'setAccessibilityLabelValue:';
    function accessibilityLabelValue: single; message 'accessibilityLabelValue';
    function accessibilityLayoutPointForScreenPoint (point: NSPoint): NSPoint; message 'accessibilityLayoutPointForScreenPoint:'; { available in 10_10 }
    function accessibilityLayoutSizeForScreenSize (size: NSSize): NSSize; message 'accessibilityLayoutSizeForScreenSize:'; { available in 10_10 }
    function accessibilityLineForIndex (index: NSInteger): NSInteger; message 'accessibilityLineForIndex:'; { available in 10_10 }
    procedure setAccessibilityLinkedUIElements(newValue: NSArray); message 'setAccessibilityLinkedUIElements:';
    function accessibilityLinkedUIElements: NSArray; message 'accessibilityLinkedUIElements';
    procedure setAccessibilityMain(newValue: ObjCBOOL); message 'setAccessibilityMain:';
    function isAccessibilityMain: ObjCBOOL; message 'isAccessibilityMain';
    procedure setAccessibilityMainWindow(newValue: id); message 'setAccessibilityMainWindow:';
    function accessibilityMainWindow: id; message 'accessibilityMainWindow';
    procedure setAccessibilityMarkerGroupUIElement(newValue: id); message 'setAccessibilityMarkerGroupUIElement:';
    function accessibilityMarkerGroupUIElement: id; message 'accessibilityMarkerGroupUIElement';
    procedure setAccessibilityMarkerTypeDescription(newValue: NSString); message 'setAccessibilityMarkerTypeDescription:';
    function accessibilityMarkerTypeDescription: NSString; message 'accessibilityMarkerTypeDescription';
    procedure setAccessibilityMarkerUIElements(newValue: NSArray); message 'setAccessibilityMarkerUIElements:';
    function accessibilityMarkerUIElements: NSArray; message 'accessibilityMarkerUIElements';
    procedure setAccessibilityMarkerValues(newValue: id); message 'setAccessibilityMarkerValues:';
    function accessibilityMarkerValues: id; message 'accessibilityMarkerValues';
    procedure setAccessibilityMaxValue(newValue: id); message 'setAccessibilityMaxValue:';
    function accessibilityMaxValue: id; message 'accessibilityMaxValue';
    procedure setAccessibilityMenuBar(newValue: id); message 'setAccessibilityMenuBar:';
    function accessibilityMenuBar: id; message 'accessibilityMenuBar';
    procedure setAccessibilityMinimizeButton(newValue: id); message 'setAccessibilityMinimizeButton:';
    function accessibilityMinimizeButton: id; message 'accessibilityMinimizeButton';
    procedure setAccessibilityMinimized(newValue: ObjCBOOL); message 'setAccessibilityMinimized:';
    function isAccessibilityMinimized: ObjCBOOL; message 'isAccessibilityMinimized';
    procedure setAccessibilityMinValue(newValue: id); message 'setAccessibilityMinValue:';
    function accessibilityMinValue: id; message 'accessibilityMinValue';
    procedure setAccessibilityModal(newValue: ObjCBOOL); message 'setAccessibilityModal:';
    function isAccessibilityModal: ObjCBOOL; message 'isAccessibilityModal';
    procedure setAccessibilityNextContents(newValue: NSArray); message 'setAccessibilityNextContents:';
    function accessibilityNextContents: NSArray; message 'accessibilityNextContents';
    procedure setAccessibilityNumberOfCharacters(newValue: NSInteger); message 'setAccessibilityNumberOfCharacters:';
    function accessibilityNumberOfCharacters: NSInteger; message 'accessibilityNumberOfCharacters';
    procedure setAccessibilityOrderedByRow(newValue: ObjCBOOL); message 'setAccessibilityOrderedByRow:';
    function isAccessibilityOrderedByRow: ObjCBOOL; message 'isAccessibilityOrderedByRow';
    procedure setAccessibilityOrientation(newValue: NSAccessibilityOrientation); message 'setAccessibilityOrientation:';
    function accessibilityOrientation: NSAccessibilityOrientation; message 'accessibilityOrientation';
    procedure setAccessibilityOverflowButton(newValue: id); message 'setAccessibilityOverflowButton:';
    function accessibilityOverflowButton: id; message 'accessibilityOverflowButton';
    procedure setAccessibilityParent(newValue: id); message 'setAccessibilityParent:';
    function accessibilityParent: id; message 'accessibilityParent';
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
    procedure setAccessibilityPlaceholderValue(newValue: NSString); message 'setAccessibilityPlaceholderValue:';
    function accessibilityPlaceholderValue: NSString; message 'accessibilityPlaceholderValue';
    procedure setAccessibilityPreviousContents(newValue: NSArray); message 'setAccessibilityPreviousContents:';
    function accessibilityPreviousContents: NSArray; message 'accessibilityPreviousContents';
    procedure setAccessibilityProtectedContent(newValue: ObjCBOOL); message 'setAccessibilityProtectedContent:';
    function isAccessibilityProtectedContent: ObjCBOOL; message 'isAccessibilityProtectedContent';
    procedure setAccessibilityProxy(newValue: id); message 'setAccessibilityProxy:';
    function accessibilityProxy: id; message 'accessibilityProxy';
    function accessibilityRangeForIndex (index: NSInteger): NSRange; message 'accessibilityRangeForIndex:'; { available in 10_10 }
    function accessibilityRangeForLine (line: NSInteger): NSRange; message 'accessibilityRangeForLine:'; { available in 10_10 }
    function accessibilityRangeForPosition (point: NSPoint): NSRange; message 'accessibilityRangeForPosition:'; { available in 10_10 }
    procedure setAccessibilityRole(newValue: NSString); message 'setAccessibilityRole:';
    function accessibilityRole: NSString; message 'accessibilityRole';
    procedure setAccessibilityRoleDescription(newValue: NSString); message 'setAccessibilityRoleDescription:';
    function accessibilityRoleDescription: NSString; message 'accessibilityRoleDescription';
    procedure setAccessibilityRowCount(newValue: NSInteger); message 'setAccessibilityRowCount:';
    function accessibilityRowCount: NSInteger; message 'accessibilityRowCount';
    procedure setAccessibilityRowHeaderUIElements(newValue: NSArray); message 'setAccessibilityRowHeaderUIElements:';
    function accessibilityRowHeaderUIElements: NSArray; message 'accessibilityRowHeaderUIElements';
    procedure setAccessibilityRowIndexRange(newValue: NSRange); message 'setAccessibilityRowIndexRange:';
    function accessibilityRowIndexRange: NSRange; message 'accessibilityRowIndexRange';
    procedure setAccessibilityRows(newValue: NSArray); message 'setAccessibilityRows:';
    function accessibilityRows: NSArray; message 'accessibilityRows';
    function accessibilityRTFForRange (range: NSRange): NSData; message 'accessibilityRTFForRange:'; { available in 10_10 }
    procedure setAccessibilityRulerMarkerType(newValue: NSAccessibilityRulerMarkerType); message 'setAccessibilityRulerMarkerType:';
    function accessibilityRulerMarkerType: NSAccessibilityRulerMarkerType; message 'accessibilityRulerMarkerType';
    function accessibilityScreenPointForLayoutPoint (point: NSPoint): NSPoint; message 'accessibilityScreenPointForLayoutPoint:'; { available in 10_10 }
    function accessibilityScreenSizeForLayoutSize (size: NSSize): NSSize; message 'accessibilityScreenSizeForLayoutSize:'; { available in 10_10 }
    procedure setAccessibilitySearchButton(newValue: id); message 'setAccessibilitySearchButton:';
    function accessibilitySearchButton: id; message 'accessibilitySearchButton';
    procedure setAccessibilitySearchMenu(newValue: id); message 'setAccessibilitySearchMenu:';
    function accessibilitySearchMenu: id; message 'accessibilitySearchMenu';
    procedure setAccessibilitySelected(newValue: ObjCBOOL); message 'setAccessibilitySelected:';
    function isAccessibilitySelected: ObjCBOOL; message 'isAccessibilitySelected';
    procedure setAccessibilitySelectedCells(newValue: NSArray); message 'setAccessibilitySelectedCells:';
    function accessibilitySelectedCells: NSArray; message 'accessibilitySelectedCells';
    procedure setAccessibilitySelectedChildren(newValue: NSArray); message 'setAccessibilitySelectedChildren:';
    function accessibilitySelectedChildren: NSArray; message 'accessibilitySelectedChildren';
    procedure setAccessibilitySelectedColumns(newValue: NSArray); message 'setAccessibilitySelectedColumns:';
    function accessibilitySelectedColumns: NSArray; message 'accessibilitySelectedColumns';
    procedure setAccessibilitySelectedRows(newValue: NSArray); message 'setAccessibilitySelectedRows:';
    function accessibilitySelectedRows: NSArray; message 'accessibilitySelectedRows';
    procedure setAccessibilitySelectedText(newValue: NSString); message 'setAccessibilitySelectedText:';
    function accessibilitySelectedText: NSString; message 'accessibilitySelectedText';
    procedure setAccessibilitySelectedTextRange(newValue: NSRange); message 'setAccessibilitySelectedTextRange:';
    function accessibilitySelectedTextRange: NSRange; message 'accessibilitySelectedTextRange';
    procedure setAccessibilitySelectedTextRanges(newValue: NSArray); message 'setAccessibilitySelectedTextRanges:';
    function accessibilitySelectedTextRanges: NSArray; message 'accessibilitySelectedTextRanges';
    procedure setAccessibilityServesAsTitleForUIElements(newValue: NSArray); message 'setAccessibilityServesAsTitleForUIElements:';
    function accessibilityServesAsTitleForUIElements: NSArray; message 'accessibilityServesAsTitleForUIElements';
    procedure setAccessibilitySharedCharacterRange(newValue: NSRange); message 'setAccessibilitySharedCharacterRange:';
    function accessibilitySharedCharacterRange: NSRange; message 'accessibilitySharedCharacterRange';
    procedure setAccessibilitySharedFocusElements(newValue: NSArray); message 'setAccessibilitySharedFocusElements:';
    function accessibilitySharedFocusElements: NSArray; message 'accessibilitySharedFocusElements';
    procedure setAccessibilitySharedTextUIElements(newValue: NSArray); message 'setAccessibilitySharedTextUIElements:';
    function accessibilitySharedTextUIElements: NSArray; message 'accessibilitySharedTextUIElements';
    procedure setAccessibilityShownMenu(newValue: id); message 'setAccessibilityShownMenu:';
    function accessibilityShownMenu: id; message 'accessibilityShownMenu';
    procedure setAccessibilitySortDirection(newValue: NSAccessibilitySortDirection); message 'setAccessibilitySortDirection:';
    function accessibilitySortDirection: NSAccessibilitySortDirection; message 'accessibilitySortDirection';
    procedure setAccessibilitySplitters(newValue: NSArray); message 'setAccessibilitySplitters:';
    function accessibilitySplitters: NSArray; message 'accessibilitySplitters';
    function accessibilityStringForRange (range: NSRange): NSString; message 'accessibilityStringForRange:'; { available in 10_10 }
    function accessibilityStyleRangeForIndex (index: NSInteger): NSRange; message 'accessibilityStyleRangeForIndex:'; { available in 10_10 }
    procedure setAccessibilitySubrole(newValue: NSString); message 'setAccessibilitySubrole:';
    function accessibilitySubrole: NSString; message 'accessibilitySubrole';
    procedure setAccessibilityTabs(newValue: NSArray); message 'setAccessibilityTabs:';
    function accessibilityTabs: NSArray; message 'accessibilityTabs';
    procedure setAccessibilityTitle(newValue: NSString); message 'setAccessibilityTitle:';
    function accessibilityTitle: NSString; message 'accessibilityTitle';
    procedure setAccessibilityTitleUIElement(newValue: id); message 'setAccessibilityTitleUIElement:';
    function accessibilityTitleUIElement: id; message 'accessibilityTitleUIElement';
    procedure setAccessibilityToolbarButton(newValue: id); message 'setAccessibilityToolbarButton:';
    function accessibilityToolbarButton: id; message 'accessibilityToolbarButton';
    procedure setAccessibilityTopLevelUIElement(newValue: id); message 'setAccessibilityTopLevelUIElement:';
    function accessibilityTopLevelUIElement: id; message 'accessibilityTopLevelUIElement';
    procedure setAccessibilityUnitDescription(newValue: NSString); message 'setAccessibilityUnitDescription:';
    function accessibilityUnitDescription: NSString; message 'accessibilityUnitDescription';
    procedure setAccessibilityUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityUnits:';
    function accessibilityUnits: NSAccessibilityUnits; message 'accessibilityUnits';
    procedure setAccessibilityURL(newValue: NSURL); message 'setAccessibilityURL:';
    function accessibilityURL: NSURL; message 'accessibilityURL';
    procedure setAccessibilityValue(newValue: id); message 'setAccessibilityValue:';
    function accessibilityValue: id; message 'accessibilityValue';
    procedure setAccessibilityValueDescription(newValue: NSString); message 'setAccessibilityValueDescription:';
    function accessibilityValueDescription: NSString; message 'accessibilityValueDescription';
    procedure setAccessibilityVerticalScrollBar(newValue: id); message 'setAccessibilityVerticalScrollBar:';
    function accessibilityVerticalScrollBar: id; message 'accessibilityVerticalScrollBar';
    procedure setAccessibilityVerticalUnitDescription(newValue: NSString); message 'setAccessibilityVerticalUnitDescription:';
    function accessibilityVerticalUnitDescription: NSString; message 'accessibilityVerticalUnitDescription';
    procedure setAccessibilityVerticalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityVerticalUnits:';
    function accessibilityVerticalUnits: NSAccessibilityUnits; message 'accessibilityVerticalUnits';
    procedure setAccessibilityVisibleCells(newValue: NSArray); message 'setAccessibilityVisibleCells:';
    function accessibilityVisibleCells: NSArray; message 'accessibilityVisibleCells';
    procedure setAccessibilityVisibleCharacterRange(newValue: NSRange); message 'setAccessibilityVisibleCharacterRange:';
    function accessibilityVisibleCharacterRange: NSRange; message 'accessibilityVisibleCharacterRange';
    procedure setAccessibilityVisibleChildren(newValue: NSArray); message 'setAccessibilityVisibleChildren:';
    function accessibilityVisibleChildren: NSArray; message 'accessibilityVisibleChildren';
    procedure setAccessibilityVisibleColumns(newValue: NSArray); message 'setAccessibilityVisibleColumns:';
    function accessibilityVisibleColumns: NSArray; message 'accessibilityVisibleColumns';
    procedure setAccessibilityVisibleRows(newValue: NSArray); message 'setAccessibilityVisibleRows:';
    function accessibilityVisibleRows: NSArray; message 'accessibilityVisibleRows';
    procedure setAccessibilityWarningValue(newValue: id); message 'setAccessibilityWarningValue:';
    function accessibilityWarningValue: id; message 'accessibilityWarningValue';
    procedure setAccessibilityWindow(newValue: id); message 'setAccessibilityWindow:';
    function accessibilityWindow: id; message 'accessibilityWindow';
    procedure setAccessibilityWindows(newValue: NSArray); message 'setAccessibilityWindows:';
    function accessibilityWindows: NSArray; message 'accessibilityWindows';
    procedure setAccessibilityZoomButton(newValue: id); message 'setAccessibilityZoomButton:';
    function accessibilityZoomButton: id; message 'accessibilityZoomButton';
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
  end;
{$endif}


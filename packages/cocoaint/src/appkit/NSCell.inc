{ Parsed from AppKit.framework NSCell.h }


{$ifdef TYPES}
type
  NSCellPtr = ^NSCell;
{$endif}

{$ifdef TYPES}
type
  NSCellType = NSUInteger;
  NSCellTypePtr = ^NSCellType;

const
  NSNullCellType = 0;
  NSTextCellType = 1;
  NSImageCellType = 2;

type
  NSCellAttribute = NSUInteger;
  NSCellAttributePtr = ^NSCellAttribute;

const
  NSCellDisabled = 0;
  NSCellState = 1;
  NSPushInCell = 2;
  NSCellEditable = 3;
  NSChangeGrayCell = 4;
  NSCellHighlighted = 5;
  NSCellLightsByContents = 6;
  NSCellLightsByGray = 7;
  NSChangeBackgroundCell = 8;
  NSCellLightsByBackground = 9;
  NSCellIsBordered = 10;
  NSCellHasOverlappingImage = 11;
  NSCellHasImageHorizontal = 12;
  NSCellHasImageOnLeftOrBottom = 13;
  NSCellChangesContents = 14;
  NSCellIsInsetButton = 15;
  NSCellAllowsMixedState = 16;

type
  NSCellImagePosition = NSUInteger;
  NSCellImagePositionPtr = ^NSCellImagePosition;

const
  NSNoImage = 0;
  NSImageOnly = 1;
  NSImageLeft = 2;
  NSImageRight = 3;
  NSImageBelow = 4;
  NSImageAbove = 5;
  NSImageOverlaps = 6;

type
  NSImageScaling = NSUInteger;
  NSImageScalingPtr = ^NSImageScaling;

const
  NSImageScaleProportionallyDown = 0;
  NSImageScaleAxesIndependently = 1;
  NSImageScaleNone = 2;
  NSImageScaleProportionallyUpOrDown = 3;
  NSScaleProportionally = 0 deprecated 'in 10_0, 10_10, "Use NSImageScaleProportionallyDown instead"';
  NSScaleToFit = 1 deprecated 'in 10_0, 10_10, "Use NSImageScaleAxesIndependently instead"';
  NSScaleNone = 2 deprecated 'in 10_0, 10_10, "Use NSImageScaleNone instead"';
const
  NSMixedState = -1;
  NSOffState = 0;
  NSOnState = 1;

type
  NSCellStateValue = NSInteger;
  NSCellStateValuePtr = ^NSCellStateValue;
  NSCellStyleMask = NSUInteger;
  NSCellStyleMaskPtr = ^NSCellStyleMask;

const
  NSNoCellMask = 0;
  NSContentsCellMask = 1;
  NSPushInCellMask = 2;
  NSChangeGrayCellMask = 4;
  NSChangeBackgroundCellMask = 8;

type
  NSControlTint = NSUInteger;
  NSControlTintPtr = ^NSControlTint;

const
  NSDefaultControlTint = 0;
  NSBlueControlTint = 1;
  NSGraphiteControlTint = 6;
  NSClearControlTint = 7;

type
  NSControlSize = NSUInteger;
  NSControlSizePtr = ^NSControlSize;

const
  NSRegularControlSize = 0;
  NSSmallControlSize = 1;
  NSMiniControlSize = 2;

type
  __CFlags = bitpacked record
    case byte of
      0: (_anonBitField___CFlags0: cuint);
      1: (
        state: 0..1;
        highlighted: 0..1;
        disabled: 0..1;
        editable: 0..1;
        type_: 0..((1 shl 2)-1);
        vCentered: 0..1;
        hCentered: 0..1;
        bordered: 0..1;
        bezeled: 0..1;
        selectable: 0..1;
        scrollable: 0..1;
        continuous: 0..1;
        actOnMouseDown: 0..1;
        isLeaf: 0..1;
        invalidObjectValue: 0..1;
        invalidFont: 0..1;
        lineBreakMode: 0..((1 shl 3)-1);
        weakTargetHelperFlag: 0..1;
        allowsAppearanceEffects: 0..1;
        singleLineMode: 0..1;
        actOnMouseDragged: 0..1;
        isLoaded: 0..1;
        truncateLastLine: 0..1;
        dontActOnMouseUp: 0..1;
        isWhite: 0..1;
        useUserKeyEquivalent: 0..1;
        showsFirstResponder: 0..1;
        focusRingType: 0..((1 shl 2)-1);
        wasSelectable: 0..1;
        hasInvalidObject: 0..1;
        allowsEditingTextAttributes: 0..1;
        importsGraphics: 0..1;
        alignment: 0..((1 shl 3)-1);
        layoutDirectionRTL: 0..1;
        backgroundStyle: 0..((1 shl 3)-1);
        cellReserved2: 0..((1 shl 4)-1);
        refusesFirstResponder: 0..1;
        needsHighlightedText: 0..1;
        dontAllowsUndo: 0..1;
        currentlyEditing: 0..1;
        allowsMixedState: 0..1;
        inMixedState: 0..1;
        sendsActionOnEndEditing: 0..1;
        inSendAction: 0..1;
        menuWasSet: 0..1;
        controlTint: 0..((1 shl 3)-1);
        controlSize: 0..((1 shl 2)-1);
        branchImageDisabled: 0..1;
        drawingInRevealover: 0..1;
        needsHighlightedTextHint: 0..1;
      );
    end;
type
  _CFlags = __CFlags;
type
  _CFlagsPtr = ^__CFlags;
type
  __CFlagsPtr = ^__CFlags;
{$endif}

{$ifdef CLASSES}

type
  NSCell = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSUserInterfaceItemIdentificationProtocol, NSAccessibilityElementProtocol, NSAccessibilityProtocol)
  private
    _contents: id;
    _cFlags: _CFlags;
    _support: id;
  public
    class function prefersTrackingUntilMouseUp: ObjCBOOL; message 'prefersTrackingUntilMouseUp';
    function initTextCell (aString: NSString): instancetype; message 'initTextCell:';
    function initImageCell (image: NSImage): instancetype; message 'initImageCell:';
    procedure setControlView(newValue: NSView); message 'setControlView:';
    function controlView: NSView; message 'controlView';
    procedure setType(newValue: NSCellType); message 'setType:';
    function type_: NSCellType; message 'type';
    procedure setState(newValue: NSInteger); message 'setState:';
    function state: NSInteger; message 'state';
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setTag(newValue: NSInteger); message 'setTag:';
    function tag: NSInteger; message 'tag';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    function sendActionOn (mask: NSInteger): NSInteger; message 'sendActionOn:';
    procedure setContinuous(newValue: ObjCBOOL); message 'setContinuous:';
    function isContinuous: ObjCBOOL; message 'isContinuous';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setSelectable(newValue: ObjCBOOL); message 'setSelectable:';
    function isSelectable: ObjCBOOL; message 'isSelectable';
    procedure setBordered(newValue: ObjCBOOL); message 'setBordered:';
    function isBordered: ObjCBOOL; message 'isBordered';
    procedure setBezeled(newValue: ObjCBOOL); message 'setBezeled:';
    function isBezeled: ObjCBOOL; message 'isBezeled';
    procedure setScrollable(newValue: ObjCBOOL); message 'setScrollable:';
    function isScrollable: ObjCBOOL; message 'isScrollable';
    procedure setHighlighted(newValue: ObjCBOOL); message 'setHighlighted:';
    function isHighlighted: ObjCBOOL; message 'isHighlighted';
    procedure setAlignment(newValue: NSTextAlignment); message 'setAlignment:';
    function alignment: NSTextAlignment; message 'alignment';
    procedure setWraps(newValue: ObjCBOOL); message 'setWraps:';
    function wraps: ObjCBOOL; message 'wraps';
    procedure setFont(newValue: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setFormatter(newValue: id); message 'setFormatter:';
    function formatter: id; message 'formatter';
    procedure setObjectValue(newValue: id); message 'setObjectValue:';
    function objectValue: id; message 'objectValue';
    function hasValidObjectValue: ObjCBOOL; message 'hasValidObjectValue';
    procedure setStringValue(newValue: NSString); message 'setStringValue:';
    function stringValue: NSString; message 'stringValue';
    function compare (otherCell: id): NSComparisonResult; message 'compare:';
    procedure setIntValue(newValue: cint); message 'setIntValue:';
    function intValue: cint; message 'intValue';
    procedure setFloatValue(newValue: single); message 'setFloatValue:';
    function floatValue: single; message 'floatValue';
    procedure setDoubleValue(newValue: double); message 'setDoubleValue:';
    function doubleValue: double; message 'doubleValue';
    procedure takeIntValueFrom (sender: id); message 'takeIntValueFrom:';
    procedure takeFloatValueFrom (sender: id); message 'takeFloatValueFrom:';
    procedure takeDoubleValueFrom (sender: id); message 'takeDoubleValueFrom:';
    procedure takeStringValueFrom (sender: id); message 'takeStringValueFrom:';
    procedure takeObjectValueFrom (sender: id); message 'takeObjectValueFrom:';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setControlTint(newValue: NSControlTint); message 'setControlTint:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlSize(newValue: NSControlSize); message 'setControlSize:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure setRepresentedObject(newValue: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    function cellAttribute (aParameter: NSCellAttribute): NSInteger; message 'cellAttribute:';
    procedure setCellAttribute_to (aParameter: NSCellAttribute; value: NSInteger); message 'setCellAttribute:to:';
    function imageRectForBounds (theRect: NSRect): NSRect; message 'imageRectForBounds:';
    function titleRectForBounds (theRect: NSRect): NSRect; message 'titleRectForBounds:';
    function drawingRectForBounds (theRect: NSRect): NSRect; message 'drawingRectForBounds:';
    function cellSize: NSSize; message 'cellSize';
    function cellSizeForBounds (aRect: NSRect): NSSize; message 'cellSizeForBounds:';
    function highlightColorWithFrame_inView (cellFrame: NSRect; controlView_: NSView): NSColor; message 'highlightColorWithFrame:inView:';
    procedure calcDrawInfo (aRect: NSRect); message 'calcDrawInfo:';
    function setUpFieldEditorAttributes (textObj: NSText): NSText; message 'setUpFieldEditorAttributes:';
    procedure drawInteriorWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawInteriorWithFrame:inView:';
    procedure drawWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawWithFrame:inView:';
    procedure highlight_withFrame_inView (flag: ObjCBOOL; cellFrame: NSRect; controlView_: NSView); message 'highlight:withFrame:inView:';
    function mouseDownFlags: NSInteger; message 'mouseDownFlags';
    procedure getPeriodicDelay_interval (delay: psingle; interval: psingle); message 'getPeriodicDelay:interval:';
    function startTrackingAt_inView (startPoint: NSPoint; controlView_: NSView): ObjCBOOL; message 'startTrackingAt:inView:';
    function continueTracking_at_inView (lastPoint: NSPoint; currentPoint: NSPoint; controlView_: NSView): ObjCBOOL; message 'continueTracking:at:inView:';
    procedure stopTracking_at_inView_mouseIsUp (lastPoint: NSPoint; stopPoint: NSPoint; controlView_: NSView; flag: ObjCBOOL); message 'stopTracking:at:inView:mouseIsUp:';
    function trackMouse_inRect_ofView_untilMouseUp (theEvent: NSEvent; cellFrame: NSRect; controlView_: NSView; flag: ObjCBOOL): ObjCBOOL; message 'trackMouse:inRect:ofView:untilMouseUp:';
    procedure editWithFrame_inView_editor_delegate_event (aRect: NSRect; controlView_: NSView; textObj: NSText; anObject: id; theEvent: NSEvent); message 'editWithFrame:inView:editor:delegate:event:';
    procedure selectWithFrame_inView_editor_delegate_start_length (aRect: NSRect; controlView_: NSView; textObj: NSText; anObject: id; selStart: NSInteger; selLength: NSInteger); message 'selectWithFrame:inView:editor:delegate:start:length:';
    procedure endEditing (textObj: NSText); message 'endEditing:';
    procedure resetCursorRect_inView (cellFrame: NSRect; controlView_: NSView); message 'resetCursorRect:inView:';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    function menuForEvent_inRect_ofView (event: NSEvent; cellFrame: NSRect; view: NSView): NSMenu; message 'menuForEvent:inRect:ofView:';
    class function defaultMenu: NSMenu; message 'defaultMenu';
    procedure setSendsActionOnEndEditing(newValue: ObjCBOOL); message 'setSendsActionOnEndEditing:';
    function sendsActionOnEndEditing: ObjCBOOL; message 'sendsActionOnEndEditing';
    procedure setBaseWritingDirection(newValue: NSWritingDirection); message 'setBaseWritingDirection:';
    function baseWritingDirection: NSWritingDirection; message 'baseWritingDirection';
    procedure setLineBreakMode(newValue: NSLineBreakMode); message 'setLineBreakMode:';
    function lineBreakMode: NSLineBreakMode; message 'lineBreakMode';
    procedure setAllowsUndo(newValue: ObjCBOOL); message 'setAllowsUndo:';
    function allowsUndo: ObjCBOOL; message 'allowsUndo';
    procedure setIntegerValue(newValue: NSInteger); message 'setIntegerValue:';
    function integerValue: NSInteger; message 'integerValue';
    procedure takeIntegerValueFrom (sender: id); message 'takeIntegerValueFrom:'; { available in 10_5 }
    procedure setTruncatesLastVisibleLine(newValue: ObjCBOOL); message 'setTruncatesLastVisibleLine:';
    function truncatesLastVisibleLine: ObjCBOOL; message 'truncatesLastVisibleLine';
    procedure setUserInterfaceLayoutDirection(newValue: NSUserInterfaceLayoutDirection); message 'setUserInterfaceLayoutDirection:';
    function userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection; message 'userInterfaceLayoutDirection';
    function fieldEditorForView (aControlView: NSView): NSTextView; message 'fieldEditorForView:'; { available in 10_6 }
    procedure setUsesSingleLineMode(newValue: ObjCBOOL); message 'setUsesSingleLineMode:';
    function usesSingleLineMode: ObjCBOOL; message 'usesSingleLineMode';
    function draggingImageComponentsWithFrame_inView (frame: NSRect; view: NSView): NSArray; message 'draggingImageComponentsWithFrame:inView:'; { available in 10_7 }

    { Adopted protocols }
		procedure setAccessibilityFrame(newValue: NSRect); message 'setAccessibilityFrame:';
    procedure setAccessibilityParent(newValue: id); message 'setAccessibilityParent:';
    procedure setAccessibilityIdentifier(newValue: NSString); message 'setAccessibilityIdentifier:';

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
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
  end;


type
  NSCell_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSCell)
    procedure setRefusesFirstResponder(newValue: ObjCBOOL); message 'setRefusesFirstResponder:';
    function refusesFirstResponder: ObjCBOOL; message 'refusesFirstResponder';
    function acceptsFirstResponder: ObjCBOOL; message 'acceptsFirstResponder';
    procedure setShowsFirstResponder(newValue: ObjCBOOL); message 'setShowsFirstResponder:';
    function showsFirstResponder: ObjCBOOL; message 'showsFirstResponder';
    procedure performClick (sender: id); message 'performClick:';
    procedure setFocusRingType(newValue: NSFocusRingType); message 'setFocusRingType:';
    function focusRingType: NSFocusRingType; message 'focusRingType';
    class function defaultFocusRingType: NSFocusRingType; message 'defaultFocusRingType';
    procedure drawFocusRingMaskWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawFocusRingMaskWithFrame:inView:'; { available in 10_7 }
    function focusRingMaskBoundsForFrame_inView (cellFrame: NSRect; controlView_: NSView): NSRect; message 'focusRingMaskBoundsForFrame:inView:'; { available in 10_7 }
    function wantsNotificationForMarkedText: ObjCBOOL; message 'wantsNotificationForMarkedText';
  end;

type
  NSCellAttributedStringMethods = objccategory external (NSCell)
    procedure setAttributedStringValue(newValue: NSAttributedString); message 'setAttributedStringValue:';
    function attributedStringValue: NSAttributedString; message 'attributedStringValue';
    procedure setAllowsEditingTextAttributes(newValue: ObjCBOOL); message 'setAllowsEditingTextAttributes:';
    function allowsEditingTextAttributes: ObjCBOOL; message 'allowsEditingTextAttributes';
    procedure setImportsGraphics(newValue: ObjCBOOL); message 'setImportsGraphics:';
    function importsGraphics: ObjCBOOL; message 'importsGraphics';
  end;

type
  NSCellMixedState = objccategory external (NSCell)
    procedure setAllowsMixedState(newValue: ObjCBOOL); message 'setAllowsMixedState:';
    function allowsMixedState: ObjCBOOL; message 'allowsMixedState';
    function nextState: NSInteger; message 'nextState';
    procedure setNextState; message 'setNextState';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSControlTintDidChangeNotification: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSCellHitResult = NSUInteger;
  NSCellHitResultPtr = ^NSCellHitResult;

const
  NSCellHitNone = 0;
  NSCellHitContentArea = 1 shl 0;
  NSCellHitEditableTextArea = 1 shl 1;
  NSCellHitTrackableArea = 1 shl 2;
{$endif}

{$ifdef CLASSES}

type
  NSCellHitTest = objccategory external (NSCell)
    function hitTestForEvent_inRect_ofView (event: NSEvent; cellFrame: NSRect; controlView_: NSView): NSCellHitResult; message 'hitTestForEvent:inRect:ofView:'; { available in 10_5 }
  end;

type
  NSCellExpansion = objccategory external (NSCell)
    function expansionFrameWithFrame_inView (cellFrame: NSRect; view: NSView): NSRect; message 'expansionFrameWithFrame:inView:'; { available in 10_5 }
    procedure drawWithExpansionFrame_inView (cellFrame: NSRect; view: NSView); message 'drawWithExpansionFrame:inView:'; { available in 10_5 }
  end;
{$endif}

{$ifdef TYPES}
type
  NSBackgroundStyle = NSInteger;
  NSBackgroundStylePtr = ^NSBackgroundStyle;

const
  NSBackgroundStyleLight = 0;
  NSBackgroundStyleDark = 1;
  NSBackgroundStyleRaised = 2;
  NSBackgroundStyleLowered = 3;
{$endif}

{$ifdef CLASSES}

type
  NSCellBackgroundStyle = objccategory external (NSCell)
    procedure setBackgroundStyle(newValue: NSBackgroundStyle); message 'setBackgroundStyle:';
    function backgroundStyle: NSBackgroundStyle; message 'backgroundStyle';
    function interiorBackgroundStyle: NSBackgroundStyle; message 'interiorBackgroundStyle';
  end;

type
  NSCell_NSDeprecated = objccategory external name 'NSDeprecated' (NSCell)
    function entryType: NSInteger; message 'entryType'; deprecated 'in 10_0, 10_0';
    procedure setEntryType (aType: NSInteger); message 'setEntryType:'; deprecated 'in 10_0, 10_0';
    function isEntryAcceptable (aString: NSString): ObjCBOOL; message 'isEntryAcceptable:'; deprecated 'in 10_0, 10_0';
    procedure setFloatingPointFormat_left_right (autoRange: ObjCBOOL; leftDigits: NSUInteger; rightDigits: NSUInteger); message 'setFloatingPointFormat:left:right:'; deprecated 'in 10_0, 10_0';
    procedure setMnemonicLocation (location: NSUInteger); message 'setMnemonicLocation:'; deprecated 'in 10_0, 10_8';
    function mnemonicLocation: NSUInteger; message 'mnemonicLocation'; deprecated 'in 10_0, 10_8';
    function mnemonic: NSString; message 'mnemonic'; deprecated 'in 10_0, 10_8';
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

{$ifdef FUNCTIONS}
procedure NSDrawThreePartImage(frame: NSRect; startCap: NSImage; centerFill: NSImage; endCap: NSImage; vertical: ObjCBOOL; op: NSCompositingOperation; alphaFraction: CGFloat; flipped: ObjCBOOL); cdecl; external; { available in 10_5 }
procedure NSDrawNinePartImage(frame: NSRect; topLeftCorner: NSImage; topEdgeFill: NSImage; topRightCorner: NSImage; leftEdgeFill: NSImage; centerFill: NSImage; rightEdgeFill: NSImage; bottomLeftCorner: NSImage; bottomEdgeFill: NSImage; bottomRightCorner: NSImage; op: NSCompositingOperation; alphaFraction: CGFloat; flipped: ObjCBOOL); cdecl; external; { available in 10_5 }
{$endif}

{$ifdef TYPES}
const
  NSAnyType = 0;
  NSIntType = 1;
  NSPositiveIntType = 2;
  NSFloatType = 3;
  NSPositiveFloatType = 4;
  NSDoubleType = 6;
  NSPositiveDoubleType = 7;
{$endif}


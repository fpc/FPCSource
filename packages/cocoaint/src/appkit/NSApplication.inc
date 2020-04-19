{ Parsed from AppKit.framework NSApplication.h }


{ Types from NSApplication }
{$ifdef TYPES}

type
  NSRequestUserAttentionType = NSUInteger;
type
  NSRequestUserAttentionTypePtr = ^NSRequestUserAttentionType;

const
  NSCriticalRequest = 0;
  NSInformationalRequest = 10;

type
  NSApplicationDelegateReply = NSUInteger;
type
  NSApplicationDelegateReplyPtr = ^NSApplicationDelegateReply;

const
  NSApplicationDelegateReplySuccess = 0;
  NSApplicationDelegateReplyCancel = 1;
  NSApplicationDelegateReplyFailure = 2;
{$endif}


{ Types from NSApplication_NSDeprecated }
{$ifdef TYPES}

const
  NSRunStoppedResponse = -1000 deprecated 'in 10_0, 10_10, "Use NSModalResponseStop instead"';
  NSRunAbortedResponse = -1001 deprecated 'in 10_0, 10_10, "Use NSModalResponseAbort instead"';
  NSRunContinuesResponse = -1002 deprecated 'in 10_0, 10_10, "Use NSModalResponseContinue instead"';
{$endif}


{$ifdef TYPES}
type
  NSApplicationPtr = ^NSApplication;
  NSApplicationDelegateProtocolPtr = ^NSApplicationDelegateProtocol;
  NSServicesMenuRequestorProtocolPtr = ^NSServicesMenuRequestorProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAppKitVersionNumber: double; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumber10_0 = 577;
  NSAppKitVersionNumber10_1 = 620;
  NSAppKitVersionNumber10_2 = 663;
  NSAppKitVersionNumber10_2_3 = 663.6;
  NSAppKitVersionNumber10_3 = 743;
  NSAppKitVersionNumber10_3_2 = 743.14;
  NSAppKitVersionNumber10_3_3 = 743.2;
  NSAppKitVersionNumber10_3_5 = 743.24;
  NSAppKitVersionNumber10_3_7 = 743.33;
  NSAppKitVersionNumber10_3_9 = 743.36;
  NSAppKitVersionNumber10_4 = 824;
  NSAppKitVersionNumber10_4_1 = 824.1;
  NSAppKitVersionNumber10_4_3 = 824.23;
  NSAppKitVersionNumber10_4_4 = 824.33;
  NSAppKitVersionNumber10_4_7 = 824.41;
  NSAppKitVersionNumber10_5 = 949;
  NSAppKitVersionNumber10_5_2 = 949.27;
  NSAppKitVersionNumber10_5_3 = 949.33;
  NSAppKitVersionNumber10_6 = 1038;
  NSAppKitVersionNumber10_7 = 1138;
  NSAppKitVersionNumber10_7_2 = 1138.23;
  NSAppKitVersionNumber10_7_3 = 1138.32;
  NSAppKitVersionNumber10_7_4 = 1138.47;
  NSAppKitVersionNumber10_8 = 1187;
  NSAppKitVersionNumber10_9 = 1265;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSModalPanelRunLoopMode: NSString; cvar; external;
  NSEventTrackingRunLoopMode: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSModalResponseStop = -1000;
  NSModalResponseAbort = -1001;
  NSModalResponseContinue = -1002;

type
  NSModalResponse = NSInteger { available in 10_9 };
  NSModalResponsePtr = ^NSModalResponse { available in 10_9 };

const
  NSUpdateWindowsRunLoopOrdering = 500000;

type
  NSApplicationPresentationOptions = NSUInteger;
  NSApplicationPresentationOptionsPtr = ^NSApplicationPresentationOptions;

const
  NSApplicationPresentationDefault = 0;
  NSApplicationPresentationAutoHideDock = 1 shl 0;
  NSApplicationPresentationHideDock = 1 shl 1;
  NSApplicationPresentationAutoHideMenuBar = 1 shl 2;
  NSApplicationPresentationHideMenuBar = 1 shl 3;
  NSApplicationPresentationDisableAppleMenu = 1 shl 4;
  NSApplicationPresentationDisableProcessSwitching = 1 shl 5;
  NSApplicationPresentationDisableForceQuit = 1 shl 6;
  NSApplicationPresentationDisableSessionTermination = 1 shl 7;
  NSApplicationPresentationDisableHideApplication = 1 shl 8;
  NSApplicationPresentationDisableMenuBarTransparency = 1 shl 9;
  NSApplicationPresentationFullScreen = 1 shl 10 { available in 10_7 };
  NSApplicationPresentationAutoHideToolbar = 1 shl 11 { available in 10_7 };

type
  NSApplicationOcclusionState = NSUInteger;
  NSApplicationOcclusionStatePtr = ^NSApplicationOcclusionState;

const
  NSApplicationOcclusionStateVisible = 1 shl 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSApp: id; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSModalSession = OpaqueType;
  _NSThreadPrivate = OpaqueType;
  _NSThreadPrivatePtr = ^_NSThreadPrivate;
{$endif}

{$ifdef CLASSES}

type
  NSApplication = objcclass external (NSResponder, NSUserInterfaceValidationsProtocol, NSAccessibilityElementProtocol, NSAccessibilityProtocol)
  private
    _currentEvent: NSEvent;
    _windowList: id;
    _keyWindow: id;
    _mainWindow: id;
    _delegate: id;
    _hiddenList: idPtr;
    _hiddenCount: cint;
    _context: NSInteger;
    _appleEventSuspensionID: pointer;
    _previousKeyWindow: id;
    _unusedApp: cshort;
    _running: cshort;
    _appFlags: bitpacked record
      case byte of
        0: (_anonBitField__appFlags0: cuint);
        1: (
          _hidden: 0..1;
          _appleEventActivationInProgress: 0..1;
          _active: 0..1;
          _hasBeenRun: 0..1;
          _doingUnhide: 0..1;
          _delegateReturnsValidRequestor: 0..1;
          _deactPending: 0..1;
          _invalidState: 0..1;
          _invalidEvent: 0..1;
          _postedWindowsNeedUpdateNote: 0..1;
          _wantsToActivate: 0..1;
          _doingHide: 0..1;
          _dontSendShouldTerminate: 0..1;
          _ignoresFullScreen: 0..1;
          _finishedLaunching: 0..1;
          _hasEventDelegate: 0..1;
          _appDying: 0..1;
          _didNSOpenOrPrint: 0..1;
          _inDealloc: 0..1;
          _pendingDidFinish: 0..1;
          _hasKeyFocus: 0..1;
          _panelsNonactivating: 0..1;
          _hiddenOnLaunch: 0..1;
          _openStatus: 0..((1 shl 2)-1);
          _batchOrdering: 0..1;
          _waitingForTerminationReply: 0..1;
          _windowMoveDisabled: 0..1;
          _enumeratingMemoryPressureHandlers: 0..1;
          _didTryRestoringPersistentState: 0..1;
          _reservedN: 0..1;
          _mightBeSwitching: 0..1;
        );
      end;
    _mainMenu: id;
    _appIcon: id;
    _unused: pointer;
    _eventDelegate: id;
    _threadingSupport: _NSThreadPrivatePtr;
  public
    class function sharedApplication: NSApplication; message 'sharedApplication';
    procedure setDelegate(newValue: NSApplicationDelegateProtocol); message 'setDelegate:';
    function delegate: NSApplicationDelegateProtocol; message 'delegate';
    function context: NSGraphicsContext; message 'context';
    procedure hide (sender: id); message 'hide:';
    procedure unhide (sender: id); message 'unhide:';
    procedure unhideWithoutActivation; message 'unhideWithoutActivation';
    function windowWithWindowNumber (windowNum: NSInteger): NSWindow; message 'windowWithWindowNumber:';
    function mainWindow: NSWindow; message 'mainWindow';
    function keyWindow: NSWindow; message 'keyWindow';
    function isActive: ObjCBOOL; message 'isActive';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isRunning: ObjCBOOL; message 'isRunning';
    procedure deactivate; message 'deactivate';
    procedure activateIgnoringOtherApps (flag: ObjCBOOL); message 'activateIgnoringOtherApps:';
    procedure hideOtherApplications (sender: id); message 'hideOtherApplications:';
    procedure unhideAllApplications (sender: id); message 'unhideAllApplications:';
    procedure finishLaunching; message 'finishLaunching';
    procedure run; message 'run';
    function runModalForWindow (theWindow: NSWindow): NSInteger; message 'runModalForWindow:';
    procedure stop (sender: id); message 'stop:';
    procedure stopModal; message 'stopModal';
    procedure stopModalWithCode (returnCode: NSInteger); message 'stopModalWithCode:';
    procedure abortModal; message 'abortModal';
    function modalWindow: NSWindow; message 'modalWindow';
    function beginModalSessionForWindow (theWindow: NSWindow): NSModalSession; message 'beginModalSessionForWindow:';
    function runModalSession (session: NSModalSession): NSInteger; message 'runModalSession:';
    procedure endModalSession (session: NSModalSession); message 'endModalSession:';
    procedure terminate (sender: id); message 'terminate:';
    function requestUserAttention (requestType: NSRequestUserAttentionType): NSInteger; message 'requestUserAttention:';
    procedure cancelUserAttentionRequest (request: NSInteger); message 'cancelUserAttentionRequest:';
    function nextEventMatchingMask_untilDate_inMode_dequeue (mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: ObjCBOOL): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure discardEventsMatchingMask_beforeEvent (mask: NSUInteger; lastEvent: NSEvent); message 'discardEventsMatchingMask:beforeEvent:';
    procedure postEvent_atStart (event: NSEvent; flag: ObjCBOOL); message 'postEvent:atStart:';
    function currentEvent: NSEvent; message 'currentEvent';
    procedure sendEvent (theEvent: NSEvent); message 'sendEvent:';
    procedure preventWindowOrdering; message 'preventWindowOrdering';
    function makeWindowsPerform_inOrder (aSelector: SEL; flag: ObjCBOOL): NSWindow; message 'makeWindowsPerform:inOrder:';
    function windows: NSArray; message 'windows';
    procedure setWindowsNeedUpdate (needUpdate: ObjCBOOL); message 'setWindowsNeedUpdate:';
    procedure updateWindows; message 'updateWindows';
    procedure setMainMenu(newValue: NSMenu); message 'setMainMenu:';
    function mainMenu: NSMenu; message 'mainMenu';
    procedure setHelpMenu(newValue: NSMenu); message 'setHelpMenu:';
    function helpMenu: NSMenu; message 'helpMenu';
    procedure setApplicationIconImage(newValue: NSImage); message 'setApplicationIconImage:';
    function applicationIconImage: NSImage; message 'applicationIconImage';
    function activationPolicy: NSApplicationActivationPolicy; message 'activationPolicy'; { available in 10_6 }
    function setActivationPolicy (activationPolicy_: NSApplicationActivationPolicy): ObjCBOOL; message 'setActivationPolicy:'; { available in 10_6 }
    function dockTile: NSDockTile; message 'dockTile';
    function sendAction_to_from (theAction: SEL; theTarget: id; sender: id): ObjCBOOL; message 'sendAction:to:from:';
    function targetForAction (theAction: SEL): id; message 'targetForAction:';
    function targetForAction_to_from (theAction: SEL; theTarget: id; sender: id): id; message 'targetForAction:to:from:';
    function tryToPerform_with (anAction: SEL; anObject: id): ObjCBOOL; message 'tryToPerform:with:';
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure reportException (theException: NSException); message 'reportException:';
    class procedure detachDrawingThread_toTarget_withObject (selector: SEL; target: id; argument: id); message 'detachDrawingThread:toTarget:withObject:';
    procedure replyToApplicationShouldTerminate (shouldTerminate: ObjCBOOL); message 'replyToApplicationShouldTerminate:';
    procedure replyToOpenOrPrint (reply: NSApplicationDelegateReply); message 'replyToOpenOrPrint:';
    procedure orderFrontCharacterPalette (sender: id); message 'orderFrontCharacterPalette:';
    procedure setPresentationOptions(newValue: NSApplicationPresentationOptions); message 'setPresentationOptions:';
    function presentationOptions: NSApplicationPresentationOptions; message 'presentationOptions';
    function currentSystemPresentationOptions: NSApplicationPresentationOptions; message 'currentSystemPresentationOptions';
    function occlusionState: NSApplicationOcclusionState; message 'occlusionState';

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
    function isAccessibilityFocused: ObjCBOOL; message 'isAccessibilityFocused';
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;


type
  NSWindowsMenu = objccategory external (NSApplication)
    procedure setWindowsMenu(newValue: NSMenu); message 'setWindowsMenu:';
    function windowsMenu: NSMenu; message 'windowsMenu';
    procedure arrangeInFront (sender: id); message 'arrangeInFront:';
    procedure removeWindowsItem (win: NSWindow); message 'removeWindowsItem:';
    procedure addWindowsItem_title_filename (win: NSWindow; aString: NSString; isFilename: ObjCBOOL); message 'addWindowsItem:title:filename:';
    procedure changeWindowsItem_title_filename (win: NSWindow; aString: NSString; isFilename: ObjCBOOL); message 'changeWindowsItem:title:filename:';
    procedure updateWindowsItem (win: NSWindow); message 'updateWindowsItem:';
    procedure miniaturizeAll (sender: id); message 'miniaturizeAll:';
  end;

type
  NSFullKeyboardAccess = objccategory external (NSApplication)
    function isFullKeyboardAccessEnabled: ObjCBOOL; message 'isFullKeyboardAccessEnabled';
  end;
{$endif}

{$ifdef TYPES}
const
  NSTerminateCancel = 0;
  NSTerminateNow = 1;
  NSTerminateLater = 2;

type
  NSApplicationTerminateReply = NSUInteger;
  NSApplicationTerminateReplyPtr = ^NSApplicationTerminateReply;

const
  NSPrintingCancelled = 0;
  NSPrintingSuccess = 1;
  NSPrintingFailure = 3;
  NSPrintingReplyLater = 2;

type
  NSApplicationPrintReply = NSUInteger;
  NSApplicationPrintReplyPtr = ^NSApplicationPrintReply;
{$endif}

{$ifdef PROTOCOLS}

type
  NSApplicationDelegateProtocol = objcprotocol external name 'NSApplicationDelegate' (NSObjectProtocol)
  optional
    function applicationShouldTerminate (sender: NSApplication): NSApplicationTerminateReply; message 'applicationShouldTerminate:';
    function application_openFile (sender: NSApplication; filename: NSString): ObjCBOOL; message 'application:openFile:';
    procedure application_openFiles (sender: NSApplication; filenames: NSArray); message 'application:openFiles:';
    function application_openTempFile (sender: NSApplication; filename: NSString): ObjCBOOL; message 'application:openTempFile:';
    function applicationShouldOpenUntitledFile (sender: NSApplication): ObjCBOOL; message 'applicationShouldOpenUntitledFile:';
    function applicationOpenUntitledFile (sender: NSApplication): ObjCBOOL; message 'applicationOpenUntitledFile:';
    function application_openFileWithoutUI (sender: id; filename: NSString): ObjCBOOL; message 'application:openFileWithoutUI:';
    function application_printFile (sender: NSApplication; filename: NSString): ObjCBOOL; message 'application:printFile:';
    function application_printFiles_withSettings_showPrintPanels (application: NSApplication; fileNames: NSArray; printSettings: NSDictionary; showPrintPanels: ObjCBOOL): NSApplicationPrintReply; message 'application:printFiles:withSettings:showPrintPanels:';
    function applicationShouldTerminateAfterLastWindowClosed (sender: NSApplication): ObjCBOOL; message 'applicationShouldTerminateAfterLastWindowClosed:';
    function applicationShouldHandleReopen_hasVisibleWindows (sender: NSApplication; flag: ObjCBOOL): ObjCBOOL; message 'applicationShouldHandleReopen:hasVisibleWindows:';
    function applicationDockMenu (sender: NSApplication): NSMenu; message 'applicationDockMenu:';
    function application_willPresentError (application: NSApplication; error: NSError): NSError; message 'application:willPresentError:';
    procedure application_didRegisterForRemoteNotificationsWithDeviceToken (application: NSApplication; deviceToken: NSData); message 'application:didRegisterForRemoteNotificationsWithDeviceToken:'; { available in 10_7 }
    procedure application_didFailToRegisterForRemoteNotificationsWithError (application: NSApplication; error: NSError); message 'application:didFailToRegisterForRemoteNotificationsWithError:'; { available in 10_7 }
    procedure application_didReceiveRemoteNotification (application: NSApplication; userInfo: NSDictionary); message 'application:didReceiveRemoteNotification:'; { available in 10_7 }
    procedure application_willEncodeRestorableState (app: NSApplication; coder: NSCoder); message 'application:willEncodeRestorableState:'; { available in 10_7 }
    procedure application_didDecodeRestorableState (app: NSApplication; coder: NSCoder); message 'application:didDecodeRestorableState:'; { available in 10_7 }
    function application_willContinueUserActivityWithType (application: NSApplication; userActivityType: NSString): ObjCBOOL; message 'application:willContinueUserActivityWithType:'; { available in 10_10 }
    function application_continueUserActivity_restorationHandler (application: NSApplication; userActivity: NSUserActivity; restorationHandler: OpaqueCBlock): ObjCBOOL; message 'application:continueUserActivity:restorationHandler:'; { available in 10_10 }
    procedure application_didFailToContinueUserActivityWithType_error (application: NSApplication; userActivityType: NSString; error: NSError); message 'application:didFailToContinueUserActivityWithType:error:'; { available in 10_10 }
    procedure application_didUpdateUserActivity (application: NSApplication; userActivity: NSUserActivity); message 'application:didUpdateUserActivity:'; { available in 10_10 }
    procedure applicationWillFinishLaunching (notification: NSNotification); message 'applicationWillFinishLaunching:';
    procedure applicationDidFinishLaunching (notification: NSNotification); message 'applicationDidFinishLaunching:';
    procedure applicationWillHide (notification: NSNotification); message 'applicationWillHide:';
    procedure applicationDidHide (notification: NSNotification); message 'applicationDidHide:';
    procedure applicationWillUnhide (notification: NSNotification); message 'applicationWillUnhide:';
    procedure applicationDidUnhide (notification: NSNotification); message 'applicationDidUnhide:';
    procedure applicationWillBecomeActive (notification: NSNotification); message 'applicationWillBecomeActive:';
    procedure applicationDidBecomeActive (notification: NSNotification); message 'applicationDidBecomeActive:';
    procedure applicationWillResignActive (notification: NSNotification); message 'applicationWillResignActive:';
    procedure applicationDidResignActive (notification: NSNotification); message 'applicationDidResignActive:';
    procedure applicationWillUpdate (notification: NSNotification); message 'applicationWillUpdate:';
    procedure applicationDidUpdate (notification: NSNotification); message 'applicationDidUpdate:';
    procedure applicationWillTerminate (notification: NSNotification); message 'applicationWillTerminate:';
    procedure applicationDidChangeScreenParameters (notification: NSNotification); message 'applicationDidChangeScreenParameters:';
    procedure applicationDidChangeOcclusionState (notification: NSNotification); message 'applicationDidChangeOcclusionState:'; { available in 10_9 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSServicesMenu = objccategory external (NSApplication)
    procedure setServicesMenu(newValue: NSMenu); message 'setServicesMenu:';
    function servicesMenu: NSMenu; message 'servicesMenu';
    procedure registerServicesMenuSendTypes_returnTypes (sendTypes: NSArray; returnTypes: NSArray); message 'registerServicesMenuSendTypes:returnTypes:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSServicesMenuRequestorProtocol = objcprotocol external name 'NSServicesMenuRequestor' (NSObjectProtocol)
  optional
    function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
    function readSelectionFromPasteboard (pboard: NSPasteboard): ObjCBOOL; message 'readSelectionFromPasteboard:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSServicesHandling = objccategory external (NSApplication)
    procedure setServicesProvider(newValue: id); message 'setServicesProvider:';
    function servicesProvider: id; message 'servicesProvider';
  end;

type
  NSStandardAboutPanel = objccategory external (NSApplication)
    procedure orderFrontStandardAboutPanel (sender: id); message 'orderFrontStandardAboutPanel:';
    procedure orderFrontStandardAboutPanelWithOptions (optionsDictionary: NSDictionary); message 'orderFrontStandardAboutPanelWithOptions:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSUserInterfaceLayoutDirection = NSInteger;
  NSUserInterfaceLayoutDirectionPtr = ^NSUserInterfaceLayoutDirection;
{$endif}

{$ifdef CLASSES}

type
  NSApplicationLayoutDirection = objccategory external (NSApplication)
    function userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection; message 'userInterfaceLayoutDirection';
  end;

type
  NSRestorableUserInterface = objccategory external (NSApplication)
    procedure disableRelaunchOnLogin; message 'disableRelaunchOnLogin'; { available in 10_7 }
    procedure enableRelaunchOnLogin; message 'enableRelaunchOnLogin'; { available in 10_7 }
  end;
{$endif}

{$ifdef TYPES}
type
  NSRemoteNotificationType = NSUInteger;
  NSRemoteNotificationTypePtr = ^NSRemoteNotificationType;
{$endif}

{$ifdef CLASSES}

type
  NSRemoteNotifications = objccategory external (NSApplication)
    procedure registerForRemoteNotificationTypes (types: NSRemoteNotificationType); message 'registerForRemoteNotificationTypes:'; { available in 10_7 }
    procedure unregisterForRemoteNotifications; message 'unregisterForRemoteNotifications'; { available in 10_7 }
    function enabledRemoteNotificationTypes: NSRemoteNotificationType; message 'enabledRemoteNotificationTypes';
  end;
{$endif}

{$ifdef FUNCTIONS}
function NSApplicationMain(argc: cint; argv: PChar { variable size array of PChar }): cint; cdecl; external;
function NSApplicationLoad: ObjCBOOL; cdecl; external;
function NSShowsServicesMenuItem(itemName: NSString): ObjCBOOL; cdecl; external;
function NSSetShowsServicesMenuItem(itemName: NSString; enabled: ObjCBOOL): NSInteger; cdecl; external;
procedure NSUpdateDynamicServices; cdecl; external;
function NSPerformService(itemName: NSString; pboard: NSPasteboard): ObjCBOOL; cdecl; external;
procedure NSRegisterServicesProvider(provider: id; name: NSString); cdecl; external;
procedure NSUnregisterServicesProvider(name: NSString); cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSApplicationDidBecomeActiveNotification: NSString; cvar; external;
  NSApplicationDidHideNotification: NSString; cvar; external;
  NSApplicationDidFinishLaunchingNotification: NSString; cvar; external;
  NSApplicationDidResignActiveNotification: NSString; cvar; external;
  NSApplicationDidUnhideNotification: NSString; cvar; external;
  NSApplicationDidUpdateNotification: NSString; cvar; external;
  NSApplicationWillBecomeActiveNotification: NSString; cvar; external;
  NSApplicationWillHideNotification: NSString; cvar; external;
  NSApplicationWillFinishLaunchingNotification: NSString; cvar; external;
  NSApplicationWillResignActiveNotification: NSString; cvar; external;
  NSApplicationWillUnhideNotification: NSString; cvar; external;
  NSApplicationWillUpdateNotification: NSString; cvar; external;
  NSApplicationWillTerminateNotification: NSString; cvar; external;
  NSApplicationDidChangeScreenParametersNotification: NSString; cvar; external;
  NSApplicationLaunchIsDefaultLaunchKey: NSString { available in 10_7 }; cvar; external;
  NSApplicationLaunchUserNotificationKey: NSString { available in 10_8 }; cvar; external;
  NSApplicationLaunchRemoteNotificationKey: NSString deprecated 'in 10_7, 10_8'; cvar; external;
  NSApplicationDidChangeOcclusionStateNotification: NSString { available in 10_9 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSApplication_NSDeprecated = objccategory external name 'NSDeprecated' (NSApplication)
    function runModalForWindow_relativeToWindow (theWindow: NSWindow; docWindow: NSWindow): NSInteger; message 'runModalForWindow:relativeToWindow:'; deprecated 'in 10_0, 10_0';
    function beginModalSessionForWindow_relativeToWindow (theWindow: NSWindow; docWindow: NSWindow): NSModalSession; message 'beginModalSessionForWindow:relativeToWindow:'; deprecated 'in 10_0, 10_0';
    procedure application_printFiles (sender: NSApplication; filenames: NSArray); message 'application:printFiles:'; deprecated 'in 10_3, 10_4';
    procedure beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo (sheet: NSWindow; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:'; deprecated 'in 10_0, 10_10, "Use -[NSWindow beginSheet:completionHandler:] instead"';
    procedure endSheet (sheet: NSWindow); message 'endSheet:'; deprecated 'in 10_0, 10_10, "Use -[NSWindow endSheet:] instead"';
    procedure endSheet_returnCode (sheet: NSWindow; returnCode: NSInteger); message 'endSheet:returnCode:'; deprecated 'in 10_0, 10_10, "Use -[NSWindow endSheet:returnCode:] instead"';
  end;
{$endif}


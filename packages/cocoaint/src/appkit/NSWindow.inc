{ Parsed from AppKit.framework NSWindow.h }


{$ifdef TYPES}
type
  NSWindowPtr = ^NSWindow;
  NSWindowDelegateProtocolPtr = ^NSWindowDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithCustomSheetPosition = 686.0;
  NSAppKitVersionNumberWithDeferredWindowDisplaySupport = 1019.0;

const
  NSBorderlessWindowMask = 0;
  NSTitledWindowMask = 1 shl 0;
  NSClosableWindowMask = 1 shl 1;
  NSMiniaturizableWindowMask = 1 shl 2;
  NSResizableWindowMask = 1 shl 3;
  NSTexturedBackgroundWindowMask = 1 shl 8;
  NSUnifiedTitleAndToolbarWindowMask = 1 shl 12;
  NSFullScreenWindowMask = 1 shl 14 { available in 10_7 };
  NSFullSizeContentViewWindowMask = 1 shl 15 { available in 10_10 };
const
  NSModalResponseOK = 1;
  NSModalResponseCancel = 0;
const
  NSDisplayWindowRunLoopOrdering = 600000;
  NSResetCursorRectsRunLoopOrdering = 700000;

type
  NSWindowSharingType = NSUInteger;
  NSWindowSharingTypePtr = ^NSWindowSharingType;

const
  NSWindowSharingNone = 0;
  NSWindowSharingReadOnly = 1;
  NSWindowSharingReadWrite = 2;

type
  NSWindowBackingLocation = NSUInteger;
  NSWindowBackingLocationPtr = ^NSWindowBackingLocation;

const
  NSWindowBackingLocationDefault = 0;
  NSWindowBackingLocationVideoMemory = 1;
  NSWindowBackingLocationMainMemory = 2;

type
  NSWindowCollectionBehavior = NSUInteger;
  NSWindowCollectionBehaviorPtr = ^NSWindowCollectionBehavior;

const
  NSWindowCollectionBehaviorDefault = 0;
  NSWindowCollectionBehaviorCanJoinAllSpaces = 1 shl 0;
  NSWindowCollectionBehaviorMoveToActiveSpace = 1 shl 1;
  NSWindowCollectionBehaviorManaged = 1 shl 2 { available in 10_6 };
  NSWindowCollectionBehaviorTransient = 1 shl 3 { available in 10_6 };
  NSWindowCollectionBehaviorStationary = 1 shl 4 { available in 10_6 };
  NSWindowCollectionBehaviorParticipatesInCycle = 1 shl 5 { available in 10_6 };
  NSWindowCollectionBehaviorIgnoresCycle = 1 shl 6 { available in 10_6 };
  NSWindowCollectionBehaviorFullScreenPrimary = 1 shl 7 { available in 10_7 };
  NSWindowCollectionBehaviorFullScreenAuxiliary = 1 shl 8 { available in 10_7 };

type
  NSWindowAnimationBehavior = NSInteger;
  NSWindowAnimationBehaviorPtr = ^NSWindowAnimationBehavior;

const
  NSWindowAnimationBehaviorDefault = 0;
  NSWindowAnimationBehaviorNone = 2;
  NSWindowAnimationBehaviorDocumentWindow = 3;
  NSWindowAnimationBehaviorUtilityWindow = 4;
  NSWindowAnimationBehaviorAlertPanel = 5;
const
  NSWindowNumberListAllApplications = 1 shl 0;
  NSWindowNumberListAllSpaces = 1 shl 4;

type
  NSWindowOcclusionState = NSUInteger;
  NSWindowOcclusionStatePtr = ^NSWindowOcclusionState;

const
  NSWindowOcclusionStateVisible = 1 shl 1;

type
  NSWindowNumberListOptions = NSUInteger;
  NSWindowNumberListOptionsPtr = ^NSWindowNumberListOptions;

const
  NSNormalWindowLevel = kCGNormalWindowLevel;
  NSFloatingWindowLevel = kCGFloatingWindowLevel;
  NSSubmenuWindowLevel = kCGTornOffMenuWindowLevel;
  NSTornOffMenuWindowLevel = kCGTornOffMenuWindowLevel;
  NSMainMenuWindowLevel = kCGMainMenuWindowLevel;
  NSStatusWindowLevel = kCGStatusWindowLevel;
  NSDockWindowLevel = kCGDockWindowLevel;
  NSModalPanelWindowLevel = kCGModalPanelWindowLevel;
  NSPopUpMenuWindowLevel = kCGPopUpMenuWindowLevel;
  NSScreenSaverWindowLevel = kCGScreenSaverWindowLevel;

type
  NSSelectionDirection = NSUInteger;
  NSSelectionDirectionPtr = ^NSSelectionDirection;

const
  NSDirectSelection = 0;
  NSSelectingNext = 1;
  NSSelectingPrevious = 2;

type
  NSWindowButton = NSUInteger;
  NSWindowButtonPtr = ^NSWindowButton;

const
  NSWindowCloseButton = 0;
  NSWindowMiniaturizeButton = 1;
  NSWindowZoomButton = 2;
  NSWindowToolbarButton = 3;
  NSWindowDocumentIconButton = 4;
  NSWindowDocumentVersionsButton = 6 { available in 10_7 };
  NSWindowFullScreenButton = 7 { available in 10_7 };

type
  NSWindowTitleVisibility = NSInteger;
  NSWindowTitleVisibilityPtr = ^NSWindowTitleVisibility;

const
  NSWindowTitleVisible = 0;
  NSWindowTitleHidden = 1;

const
  NSEventDurationForever = DBL_MAX;
{$endif}

{$ifdef CLASSES}

type
  NSWindow = objcclass external (NSResponder, NSAnimatablePropertyContainerProtocol, NSUserInterfaceValidationsProtocol, NSUserInterfaceItemIdentificationProtocol, NSAppearanceCustomizationProtocol, NSAccessibilityElementProtocol, NSAccessibilityProtocol)
  private
    _frame: NSRect;
    _contentView: id;
    _delegate: id;
    _firstResponder: NSResponder;
    _lastLeftHit: NSView;
    _lastRightHit: NSView;
    _counterpart: id;
    _fieldEditor: id;
    _winEventMask: cint;
    _windowNum: NSInteger;
    _level: cint;
    _backgroundColor: NSColor;
    _borderView: id;
    _postingDisabled: char;
    _styleMask: char;
    _flushDisabled: char;
    _reservedWindow1: char;
    _cursorRects: pointer;
    _trectTable: pointer;
    _miniIcon: NSImage;
    _bamboo: cint32;
    _dragTypes: NSMutableSet;
    _representedURL: NSURL;
    _sizeLimits: NSSizePtr;
    _frameSaveName: NSString;
    _reservedWindow2: id;
    _wFlags: bitpacked record
      case byte of
        0: (_anonBitField__wFlags0: cuint);
        1: (
          backing: 0..((1 shl 2)-1);
          visible: 0..1;
          isMainWindow: 0..1;
          isKeyWindow: 0..1;
          hidesOnDeactivate: 0..1;
          dontFreeWhenClosed: 0..1;
          oneShot: 0..1;
          deferred: 0..1;
          cursorRectsDisabled: 0..1;
          haveFreeCursorRects: 0..1;
          validCursorRects: 0..1;
          docEdited: 0..1;
          dynamicDepthLimit: 0..1;
          worksWhenModal: 0..1;
          limitedBecomeKey: 0..1;
          needsFlush: 0..1;
          viewsNeedDisplay: 0..1;
          ignoredFirstMouse: 0..1;
          repostedFirstMouse: 0..1;
          windowDying: 0..1;
          tempHidden: 0..1;
          floatingPanel: 0..1;
          wantsToBeOnMainScreen: 0..1;
          optimizedDrawingOk: 0..1;
          optimizeDrawing: 0..1;
          titleIsRepresentedFilename: 0..1;
          excludedFromWindowsMenu: 0..1;
          depthLimit: 0..((1 shl 4)-1);
          delegateReturnsValidRequestor: 0..1;
          lmouseupPending: 0..1;
          rmouseupPending: 0..1;
          wantsToDestroyRealWindow: 0..1;
          wantsToRegDragTypes: 0..1;
          sentInvalidateCursorRectsMsg: 0..1;
          avoidsActivation: 0..1;
          frameSavedUsingTitle: 0..1;
          didRegDragTypes: 0..1;
          delayedOneShot: 0..1;
          postedNeedsDisplayNote: 0..1;
          postedInvalidCursorRectsNote: 0..1;
          initialFirstResponderTempSet: 0..1;
          autodisplay: 0..1;
          tossedFirstEvent: 0..1;
          isImageCache: 0..1;
          autolayoutEngagedSomewhere: 0..1;
          hasRegisteredBackdropViews: 0..1;
          _unused: 0..1;
          keyViewSelectionDirection: 0..((1 shl 2)-1);
          defaultButtonCellKETemporarilyDisabled: 0..1;
          defaultButtonCellKEDisabled: 0..1;
          menuHasBeenSet: 0..1;
          wantsToBeModal: 0..1;
          showingModalFrame: 0..1;
          isTerminating: 0..1;
          makingFirstResponderForMouseDown: 0..1;
          needsZoom: 0..1;
          sentWindowNeedsDisplayMsg: 0..1;
          unused: 0..((1 shl 2)-1);
        );
      end;
    _defaultButtonCell: id;
    _initialFirstResponder: NSView;
    _auxiliaryStorage: NSWindowAuxiliary;
  public
    class function frameRectForContentRect_styleMask (cRect: NSRect; aStyle: NSUInteger): NSRect; message 'frameRectForContentRect:styleMask:';
    class function contentRectForFrameRect_styleMask (fRect: NSRect; aStyle: NSUInteger): NSRect; message 'contentRectForFrameRect:styleMask:';
    class function minFrameWidthWithTitle_styleMask (aTitle: NSString; aStyle: NSUInteger): CGFloat; message 'minFrameWidthWithTitle:styleMask:';
    class function defaultDepthLimit: NSWindowDepth; message 'defaultDepthLimit';
    function frameRectForContentRect (contentRect: NSRect): NSRect; message 'frameRectForContentRect:';
    function contentRectForFrameRect (frameRect: NSRect): NSRect; message 'contentRectForFrameRect:';
    function initWithContentRect_styleMask_backing_defer (contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBOOL): instancetype; message 'initWithContentRect:styleMask:backing:defer:';
    function initWithContentRect_styleMask_backing_defer_screen (contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBOOL; screen: NSScreen): instancetype; message 'initWithContentRect:styleMask:backing:defer:screen:';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setTitleVisibility(newValue: NSWindowTitleVisibility); message 'setTitleVisibility:';
    function titleVisibility: NSWindowTitleVisibility; message 'titleVisibility';
    procedure setTitlebarAppearsTransparent(newValue: ObjCBOOL); message 'setTitlebarAppearsTransparent:';
    function titlebarAppearsTransparent: ObjCBOOL; message 'titlebarAppearsTransparent';
    function contentLayoutRect: NSRect; message 'contentLayoutRect';
    function contentLayoutGuide: id; message 'contentLayoutGuide';
    procedure setTitlebarAccessoryViewControllers(newValue: NSArray); message 'setTitlebarAccessoryViewControllers:';
    function titlebarAccessoryViewControllers: NSArray; message 'titlebarAccessoryViewControllers';
    procedure addTitlebarAccessoryViewController (childViewController: NSTitlebarAccessoryViewController); message 'addTitlebarAccessoryViewController:'; { available in 10_10 }
    procedure insertTitlebarAccessoryViewController_atIndex (childViewController: NSTitlebarAccessoryViewController; index: NSInteger); message 'insertTitlebarAccessoryViewController:atIndex:'; { available in 10_10 }
    procedure removeTitlebarAccessoryViewControllerAtIndex (index: NSInteger); message 'removeTitlebarAccessoryViewControllerAtIndex:'; { available in 10_10 }
    procedure setRepresentedURL(newValue: NSURL); message 'setRepresentedURL:';
    function representedURL: NSURL; message 'representedURL';
    procedure setRepresentedFilename(newValue: NSString); message 'setRepresentedFilename:';
    function representedFilename: NSString; message 'representedFilename';
    procedure setTitleWithRepresentedFilename (filename: NSString); message 'setTitleWithRepresentedFilename:';
    procedure setExcludedFromWindowsMenu(newValue: ObjCBOOL); message 'setExcludedFromWindowsMenu:';
    function isExcludedFromWindowsMenu: ObjCBOOL; message 'isExcludedFromWindowsMenu';
    procedure setContentView(newValue: id); message 'setContentView:';
    function contentView: id; message 'contentView';
    procedure setDelegate(newValue: NSWindowDelegateProtocol); message 'setDelegate:';
    function delegate: NSWindowDelegateProtocol; message 'delegate';
    function windowNumber: NSInteger; message 'windowNumber';
    procedure setStyleMask(newValue: NSUInteger); message 'setStyleMask:';
    function styleMask: NSUInteger; message 'styleMask';
    function fieldEditor_forObject (createFlag: ObjCBOOL; anObject: id): NSText; message 'fieldEditor:forObject:';
    procedure endEditingFor (anObject: id); message 'endEditingFor:';
    function constrainFrameRect_toScreen (frameRect: NSRect; screen: NSScreen): NSRect; message 'constrainFrameRect:toScreen:';
    procedure setFrame_display (frameRect: NSRect; flag: ObjCBOOL); message 'setFrame:display:';
    procedure setContentSize (aSize: NSSize); message 'setContentSize:';
    procedure setFrameOrigin (aPoint: NSPoint); message 'setFrameOrigin:';
    procedure setFrameTopLeftPoint (aPoint: NSPoint); message 'setFrameTopLeftPoint:';
    function cascadeTopLeftFromPoint (topLeftPoint: NSPoint): NSPoint; message 'cascadeTopLeftFromPoint:';
    function frame: NSRect; message 'frame';
    function animationResizeTime (newFrame: NSRect): NSTimeInterval; message 'animationResizeTime:';
    procedure setFrame_display_animate (frameRect: NSRect; displayFlag: ObjCBOOL; animateFlag: ObjCBOOL); message 'setFrame:display:animate:';
    function inLiveResize: ObjCBOOL; message 'inLiveResize';
    procedure setShowsResizeIndicator(newValue: ObjCBOOL); message 'setShowsResizeIndicator:';
    function showsResizeIndicator: ObjCBOOL; message 'showsResizeIndicator';
    procedure setResizeIncrements(newValue: NSSize); message 'setResizeIncrements:';
    function resizeIncrements: NSSize; message 'resizeIncrements';
    procedure setAspectRatio(newValue: NSSize); message 'setAspectRatio:';
    function aspectRatio: NSSize; message 'aspectRatio';
    procedure setContentResizeIncrements(newValue: NSSize); message 'setContentResizeIncrements:';
    function contentResizeIncrements: NSSize; message 'contentResizeIncrements';
    procedure setContentAspectRatio(newValue: NSSize); message 'setContentAspectRatio:';
    function contentAspectRatio: NSSize; message 'contentAspectRatio';
    procedure disableFlushWindow; message 'disableFlushWindow';
    procedure enableFlushWindow; message 'enableFlushWindow';
    function isFlushWindowDisabled: ObjCBOOL; message 'isFlushWindowDisabled';
    procedure flushWindow; message 'flushWindow';
    procedure flushWindowIfNeeded; message 'flushWindowIfNeeded';
    procedure setViewsNeedDisplay(newValue: ObjCBOOL); message 'setViewsNeedDisplay:';
    function viewsNeedDisplay: ObjCBOOL; message 'viewsNeedDisplay';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure display; message 'display';
    procedure setAutodisplay(newValue: ObjCBOOL); message 'setAutodisplay:';
    function isAutodisplay: ObjCBOOL; message 'isAutodisplay';
    procedure setPreservesContentDuringLiveResize(newValue: ObjCBOOL); message 'setPreservesContentDuringLiveResize:';
    function preservesContentDuringLiveResize: ObjCBOOL; message 'preservesContentDuringLiveResize';
    procedure update; message 'update';
    function makeFirstResponder (aResponder: NSResponder): ObjCBOOL; message 'makeFirstResponder:';
    function firstResponder: NSResponder; message 'firstResponder';
    function resizeFlags: NSInteger; message 'resizeFlags';
    procedure keyDown (theEvent: NSEvent); message 'keyDown:';
    procedure close; message 'close';
    procedure setReleasedWhenClosed(newValue: ObjCBOOL); message 'setReleasedWhenClosed:';
    function isReleasedWhenClosed: ObjCBOOL; message 'isReleasedWhenClosed';
    procedure miniaturize (sender: id); message 'miniaturize:';
    procedure deminiaturize (sender: id); message 'deminiaturize:';
    function isZoomed: ObjCBOOL; message 'isZoomed';
    procedure zoom (sender: id); message 'zoom:';
    function isMiniaturized: ObjCBOOL; message 'isMiniaturized';
    function tryToPerform_with (anAction: SEL; anObject: id): ObjCBOOL; message 'tryToPerform:with:';
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setContentBorderThickness_forEdge (thickness: CGFloat; edge: NSRectEdge); message 'setContentBorderThickness:forEdge:'; { available in 10_5 }
    function contentBorderThicknessForEdge (edge: NSRectEdge): CGFloat; message 'contentBorderThicknessForEdge:'; { available in 10_5 }
    procedure setAutorecalculatesContentBorderThickness_forEdge (flag: ObjCBOOL; edge: NSRectEdge); message 'setAutorecalculatesContentBorderThickness:forEdge:'; { available in 10_5 }
    function autorecalculatesContentBorderThicknessForEdge (edge: NSRectEdge): ObjCBOOL; message 'autorecalculatesContentBorderThicknessForEdge:'; { available in 10_5 }
    procedure setMovable(newValue: ObjCBOOL); message 'setMovable:';
    function isMovable: ObjCBOOL; message 'isMovable';
    procedure setMovableByWindowBackground(newValue: ObjCBOOL); message 'setMovableByWindowBackground:';
    function isMovableByWindowBackground: ObjCBOOL; message 'isMovableByWindowBackground';
    procedure setHidesOnDeactivate(newValue: ObjCBOOL); message 'setHidesOnDeactivate:';
    function hidesOnDeactivate: ObjCBOOL; message 'hidesOnDeactivate';
    procedure setCanHide(newValue: ObjCBOOL); message 'setCanHide:';
    function canHide: ObjCBOOL; message 'canHide';
    procedure center; message 'center';
    procedure makeKeyAndOrderFront (sender: id); message 'makeKeyAndOrderFront:';
    procedure orderFront (sender: id); message 'orderFront:';
    procedure orderBack (sender: id); message 'orderBack:';
    procedure orderOut (sender: id); message 'orderOut:';
    procedure orderWindow_relativeTo (place: NSWindowOrderingMode; otherWin: NSInteger); message 'orderWindow:relativeTo:';
    procedure orderFrontRegardless; message 'orderFrontRegardless';
    procedure setMiniwindowImage(newValue: NSImage); message 'setMiniwindowImage:';
    function miniwindowImage: NSImage; message 'miniwindowImage';
    procedure setMiniwindowTitle(newValue: NSString); message 'setMiniwindowTitle:';
    function miniwindowTitle: NSString; message 'miniwindowTitle';
    function dockTile: NSDockTile; message 'dockTile';
    procedure setDocumentEdited(newValue: ObjCBOOL); message 'setDocumentEdited:';
    function isDocumentEdited: ObjCBOOL; message 'isDocumentEdited';
    function isVisible: ObjCBOOL; message 'isVisible';
    function isKeyWindow: ObjCBOOL; message 'isKeyWindow';
    function isMainWindow: ObjCBOOL; message 'isMainWindow';
    function canBecomeKeyWindow: ObjCBOOL; message 'canBecomeKeyWindow';
    function canBecomeMainWindow: ObjCBOOL; message 'canBecomeMainWindow';
    procedure makeKeyWindow; message 'makeKeyWindow';
    procedure makeMainWindow; message 'makeMainWindow';
    procedure becomeKeyWindow; message 'becomeKeyWindow';
    procedure resignKeyWindow; message 'resignKeyWindow';
    procedure becomeMainWindow; message 'becomeMainWindow';
    procedure resignMainWindow; message 'resignMainWindow';
    function worksWhenModal: ObjCBOOL; message 'worksWhenModal';
    procedure setPreventsApplicationTerminationWhenModal(newValue: ObjCBOOL); message 'setPreventsApplicationTerminationWhenModal:';
    function preventsApplicationTerminationWhenModal: ObjCBOOL; message 'preventsApplicationTerminationWhenModal';
    function convertRectToScreen (aRect: NSRect): NSRect; message 'convertRectToScreen:'; { available in 10_7 }
    function convertRectFromScreen (aRect: NSRect): NSRect; message 'convertRectFromScreen:'; { available in 10_7 }
    function convertRectToBacking (aRect: NSRect): NSRect; message 'convertRectToBacking:'; { available in 10_7 }
    function convertRectFromBacking (aRect: NSRect): NSRect; message 'convertRectFromBacking:'; { available in 10_7 }
    function backingAlignedRect_options (aRect: NSRect; options: NSAlignmentOptions): NSRect; message 'backingAlignedRect:options:'; { available in 10_7 }
    function backingScaleFactor: CGFloat; message 'backingScaleFactor';
    procedure performClose (sender: id); message 'performClose:';
    procedure performMiniaturize (sender: id); message 'performMiniaturize:';
    procedure performZoom (sender: id); message 'performZoom:';
    procedure setOneShot(newValue: ObjCBOOL); message 'setOneShot:';
    function isOneShot: ObjCBOOL; message 'isOneShot';
    function dataWithEPSInsideRect (rect: NSRect): NSData; message 'dataWithEPSInsideRect:';
    function dataWithPDFInsideRect (rect: NSRect): NSData; message 'dataWithPDFInsideRect:';
    procedure print (sender: id); message 'print:';
    procedure disableCursorRects; message 'disableCursorRects';
    procedure enableCursorRects; message 'enableCursorRects';
    procedure discardCursorRects; message 'discardCursorRects';
    function areCursorRectsEnabled: ObjCBOOL; message 'areCursorRectsEnabled';
    procedure invalidateCursorRectsForView (aView: NSView); message 'invalidateCursorRectsForView:';
    procedure resetCursorRects; message 'resetCursorRects';
    procedure setAllowsToolTipsWhenApplicationIsInactive(newValue: ObjCBOOL); message 'setAllowsToolTipsWhenApplicationIsInactive:';
    function allowsToolTipsWhenApplicationIsInactive: ObjCBOOL; message 'allowsToolTipsWhenApplicationIsInactive';
    procedure setBackingType(newValue: NSBackingStoreType); message 'setBackingType:';
    function backingType: NSBackingStoreType; message 'backingType';
    procedure setLevel(newValue: NSInteger); message 'setLevel:';
    function level: NSInteger; message 'level';
    procedure setDepthLimit(newValue: NSWindowDepth); message 'setDepthLimit:';
    function depthLimit: NSWindowDepth; message 'depthLimit';
    procedure setDynamicDepthLimit (flag: ObjCBOOL); message 'setDynamicDepthLimit:';
    function hasDynamicDepthLimit: ObjCBOOL; message 'hasDynamicDepthLimit';
    function screen: NSScreen; message 'screen';
    function deepestScreen: NSScreen; message 'deepestScreen';
    procedure setHasShadow(newValue: ObjCBOOL); message 'setHasShadow:';
    function hasShadow: ObjCBOOL; message 'hasShadow';
    procedure invalidateShadow; message 'invalidateShadow';
    procedure setAlphaValue(newValue: CGFloat); message 'setAlphaValue:';
    function alphaValue: CGFloat; message 'alphaValue';
    procedure setOpaque(newValue: ObjCBOOL); message 'setOpaque:';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setSharingType(newValue: NSWindowSharingType); message 'setSharingType:';
    function sharingType: NSWindowSharingType; message 'sharingType';
    procedure setPreferredBackingLocation(newValue: NSWindowBackingLocation); message 'setPreferredBackingLocation:';
    function preferredBackingLocation: NSWindowBackingLocation; message 'preferredBackingLocation';
    function backingLocation: NSWindowBackingLocation; message 'backingLocation';
    procedure setAllowsConcurrentViewDrawing(newValue: ObjCBOOL); message 'setAllowsConcurrentViewDrawing:';
    function allowsConcurrentViewDrawing: ObjCBOOL; message 'allowsConcurrentViewDrawing';
    procedure setDisplaysWhenScreenProfileChanges(newValue: ObjCBOOL); message 'setDisplaysWhenScreenProfileChanges:';
    function displaysWhenScreenProfileChanges: ObjCBOOL; message 'displaysWhenScreenProfileChanges';
    procedure disableScreenUpdatesUntilFlush; message 'disableScreenUpdatesUntilFlush';
    procedure setCanBecomeVisibleWithoutLogin(newValue: ObjCBOOL); message 'setCanBecomeVisibleWithoutLogin:';
    function canBecomeVisibleWithoutLogin: ObjCBOOL; message 'canBecomeVisibleWithoutLogin';
    procedure setCollectionBehavior(newValue: NSWindowCollectionBehavior); message 'setCollectionBehavior:';
    function collectionBehavior: NSWindowCollectionBehavior; message 'collectionBehavior';
    procedure setAnimationBehavior(newValue: NSWindowAnimationBehavior); message 'setAnimationBehavior:';
    function animationBehavior: NSWindowAnimationBehavior; message 'animationBehavior';
    function isOnActiveSpace: ObjCBOOL; message 'isOnActiveSpace';
    procedure toggleFullScreen (sender: id); message 'toggleFullScreen:'; { available in 10_7 }
    function stringWithSavedFrame: NSString; message 'stringWithSavedFrame';
    procedure setFrameFromString (string_: NSString); message 'setFrameFromString:';
    procedure saveFrameUsingName (name: NSString); message 'saveFrameUsingName:';
    function setFrameUsingName_force (name: NSString; force: ObjCBOOL): ObjCBOOL; message 'setFrameUsingName:force:';
    function setFrameUsingName (name: NSString): ObjCBOOL; message 'setFrameUsingName:';
    function setFrameAutosaveName (name: NSString): ObjCBOOL; message 'setFrameAutosaveName:';
    function frameAutosaveName: NSString; message 'frameAutosaveName';
    class procedure removeFrameUsingName (name: NSString); message 'removeFrameUsingName:';
    procedure cacheImageInRect (aRect: NSRect); message 'cacheImageInRect:';
    procedure restoreCachedImage; message 'restoreCachedImage';
    procedure discardCachedImage; message 'discardCachedImage';
    procedure setMinSize(newValue: NSSize); message 'setMinSize:';
    function minSize: NSSize; message 'minSize';
    procedure setMaxSize(newValue: NSSize); message 'setMaxSize:';
    function maxSize: NSSize; message 'maxSize';
    procedure setContentMinSize(newValue: NSSize); message 'setContentMinSize:';
    function contentMinSize: NSSize; message 'contentMinSize';
    procedure setContentMaxSize(newValue: NSSize); message 'setContentMaxSize:';
    function contentMaxSize: NSSize; message 'contentMaxSize';
    procedure trackEventsMatchingMask_timeout_mode_handler (mask: NSEventMask; timeout: NSTimeInterval; mode: NSString; trackingHandler: OpaqueCBlock); message 'trackEventsMatchingMask:timeout:mode:handler:'; { available in 10_10 }
    function nextEventMatchingMask (mask: NSUInteger): NSEvent; message 'nextEventMatchingMask:';
    function nextEventMatchingMask_untilDate_inMode_dequeue (mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: ObjCBOOL): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure discardEventsMatchingMask_beforeEvent (mask: NSUInteger; lastEvent: NSEvent); message 'discardEventsMatchingMask:beforeEvent:';
    procedure postEvent_atStart (event: NSEvent; flag: ObjCBOOL); message 'postEvent:atStart:';
    function currentEvent: NSEvent; message 'currentEvent';
    procedure setAcceptsMouseMovedEvents(newValue: ObjCBOOL); message 'setAcceptsMouseMovedEvents:';
    function acceptsMouseMovedEvents: ObjCBOOL; message 'acceptsMouseMovedEvents';
    procedure setIgnoresMouseEvents(newValue: ObjCBOOL); message 'setIgnoresMouseEvents:';
    function ignoresMouseEvents: ObjCBOOL; message 'ignoresMouseEvents';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    procedure sendEvent (theEvent: NSEvent); message 'sendEvent:';
    function mouseLocationOutsideOfEventStream: NSPoint; message 'mouseLocationOutsideOfEventStream';
    class procedure menuChanged (menu_: NSMenu); message 'menuChanged:';
    function windowController: id; message 'windowController';
    procedure setWindowController (windowController_: NSWindowController); message 'setWindowController:';
    procedure beginSheet_completionHandler (sheetWindow: NSWindow; handler: OpaqueCBlock); message 'beginSheet:completionHandler:'; { available in 10_9 }
    procedure beginCriticalSheet_completionHandler (sheetWindow: NSWindow; handler: OpaqueCBlock); message 'beginCriticalSheet:completionHandler:'; { available in 10_9 }
    procedure endSheet (sheetWindow: NSWindow); message 'endSheet:'; { available in 10_9 }
    procedure endSheet_returnCode (sheetWindow: NSWindow; returnCode: NSModalResponse); message 'endSheet:returnCode:'; { available in 10_9 }
    function sheets: NSArray; message 'sheets';
    function attachedSheet: NSWindow; message 'attachedSheet';
    function isSheet: ObjCBOOL; message 'isSheet';
    function sheetParent: NSWindow; message 'sheetParent';
    class function standardWindowButton_forStyleMask (b: NSWindowButton; styleMask_: NSUInteger): NSButton; message 'standardWindowButton:forStyleMask:';
    function standardWindowButton (b: NSWindowButton): NSButton; message 'standardWindowButton:';
    procedure addChildWindow_ordered (childWin: NSWindow; place: NSWindowOrderingMode); message 'addChildWindow:ordered:';
    procedure removeChildWindow (childWin: NSWindow); message 'removeChildWindow:';
    function childWindows: NSArray; message 'childWindows';
    procedure setParentWindow(newValue: NSWindow); message 'setParentWindow:';
    function parentWindow: NSWindow; message 'parentWindow';
    function graphicsContext: NSGraphicsContext; message 'graphicsContext';
    procedure setColorSpace(newValue: NSColorSpace); message 'setColorSpace:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    class function windowNumbersWithOptions (options: NSWindowNumberListOptions): NSArray; message 'windowNumbersWithOptions:'; { available in 10_6 }
    class function windowNumberAtPoint_belowWindowWithWindowNumber (point: NSPoint; windowNumber_: NSInteger): NSInteger; message 'windowNumberAtPoint:belowWindowWithWindowNumber:'; { available in 10_6 }
    function occlusionState: NSWindowOcclusionState; message 'occlusionState';
    procedure setContentViewController(newValue: NSViewController); message 'setContentViewController:';
    function contentViewController: NSViewController; message 'contentViewController';
    class function windowWithContentViewController (contentViewController_: NSViewController): instancetype; message 'windowWithContentViewController:'; { available in 10_10 }

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
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    function animator: instancetype; message 'animator'; { available in 10_5 }
    procedure setAppearance(newValue: NSAppearance); message 'setAppearance:';
    function appearance: NSAppearance; message 'appearance';
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;


type
  NSWindow_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSWindow)
    procedure setInitialFirstResponder(newValue: NSView); message 'setInitialFirstResponder:';
    function initialFirstResponder: NSView; message 'initialFirstResponder';
    procedure selectNextKeyView (sender: id); message 'selectNextKeyView:';
    procedure selectPreviousKeyView (sender: id); message 'selectPreviousKeyView:';
    procedure selectKeyViewFollowingView (aView: NSView); message 'selectKeyViewFollowingView:';
    procedure selectKeyViewPrecedingView (aView: NSView); message 'selectKeyViewPrecedingView:';
    function keyViewSelectionDirection: NSSelectionDirection; message 'keyViewSelectionDirection';
    procedure setDefaultButtonCell (defButt: NSButtonCell); message 'setDefaultButtonCell:';
    function defaultButtonCell: NSButtonCell; message 'defaultButtonCell';
    procedure disableKeyEquivalentForDefaultButtonCell; message 'disableKeyEquivalentForDefaultButtonCell';
    procedure enableKeyEquivalentForDefaultButtonCell; message 'enableKeyEquivalentForDefaultButtonCell';
    procedure setAutorecalculatesKeyViewLoop(newValue: ObjCBOOL); message 'setAutorecalculatesKeyViewLoop:';
    function autorecalculatesKeyViewLoop: ObjCBOOL; message 'autorecalculatesKeyViewLoop';
    procedure recalculateKeyViewLoop; message 'recalculateKeyViewLoop';
  end;

type
  NSToolbarSupport = objccategory external (NSWindow)
    procedure setToolbar(newValue: NSToolbar); message 'setToolbar:';
    function toolbar: NSToolbar; message 'toolbar';
    procedure toggleToolbarShown (sender: id); message 'toggleToolbarShown:';
    procedure runToolbarCustomizationPalette (sender: id); message 'runToolbarCustomizationPalette:';
    procedure setShowsToolbarButton(newValue: ObjCBOOL); message 'setShowsToolbarButton:';
    function showsToolbarButton: ObjCBOOL; message 'showsToolbarButton';
  end;

type
  NSDrag = objccategory external (NSWindow)
    procedure dragImage_at_offset_event_pasteboard_source_slideBack (anImage: NSImage; baseLocation: NSPoint; initialOffset: NSSize; event: NSEvent; pboard: NSPasteboard; sourceObj: id; slideFlag: ObjCBOOL); message 'dragImage:at:offset:event:pasteboard:source:slideBack:';
    procedure registerForDraggedTypes (newTypes: NSArray); message 'registerForDraggedTypes:';
    procedure unregisterDraggedTypes; message 'unregisterDraggedTypes';
  end;

type
  NSCarbonExtensions = objccategory external (NSWindow)
    function initWithWindowRef (windowRef: pointer): NSWindow; message 'initWithWindowRef:';
    function windowRef: pointer; message 'windowRef';
  end;

type
  NSWindow_NSDeprecated = objccategory external name 'NSDeprecated' (NSWindow)
    function gState: NSInteger; message 'gState'; deprecated 'in 10_0, 10_10';
    function convertBaseToScreen (aPoint: NSPoint): NSPoint; message 'convertBaseToScreen:'; deprecated 'in 10_0, 10_7, "Use -convertRectToScreen: instead"';
    function convertScreenToBase (aPoint: NSPoint): NSPoint; message 'convertScreenToBase:'; deprecated 'in 10_0, 10_7, "Use -convertRectFromScreen: instead"';
    function canBeVisibleOnAllSpaces: ObjCBOOL; message 'canBeVisibleOnAllSpaces'; deprecated 'in 10_5, 10_5';
    procedure setCanBeVisibleOnAllSpaces (flag: ObjCBOOL); message 'setCanBeVisibleOnAllSpaces:'; deprecated 'in 10_5, 10_5';
    function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor'; deprecated 'in 10_4, 10_7, "Use -convertRectToBacking: and -backingScaleFactor instead"';
    procedure useOptimizedDrawing (flag: ObjCBOOL); message 'useOptimizedDrawing:'; deprecated 'in 10_0, 10_10';
    function canStoreColor: ObjCBOOL; message 'canStoreColor'; deprecated 'in 10_0, 10_10';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSWindowDelegateProtocol = objcprotocol external name 'NSWindowDelegate' (NSObjectProtocol)
  optional
    function windowShouldClose (sender: id): ObjCBOOL; message 'windowShouldClose:';
    function windowWillReturnFieldEditor_toObject (sender: NSWindow; client: id): id; message 'windowWillReturnFieldEditor:toObject:';
    function windowWillResize_toSize (sender: NSWindow; frameSize: NSSize): NSSize; message 'windowWillResize:toSize:';
    function windowWillUseStandardFrame_defaultFrame (window: NSWindow; newFrame: NSRect): NSRect; message 'windowWillUseStandardFrame:defaultFrame:';
    function windowShouldZoom_toFrame (window: NSWindow; newFrame: NSRect): ObjCBOOL; message 'windowShouldZoom:toFrame:';
    function windowWillReturnUndoManager (window: NSWindow): NSUndoManager; message 'windowWillReturnUndoManager:';
    function window_willPositionSheet_usingRect (window: NSWindow; sheet: NSWindow; rect: NSRect): NSRect; message 'window:willPositionSheet:usingRect:';
    function window_shouldPopUpDocumentPathMenu (window: NSWindow; menu: NSMenu): ObjCBOOL; message 'window:shouldPopUpDocumentPathMenu:'; { available in 10_5 }
    function window_shouldDragDocumentWithEvent_from_withPasteboard (window: NSWindow; event: NSEvent; dragImageLocation: NSPoint; pasteboard: NSPasteboard): ObjCBOOL; message 'window:shouldDragDocumentWithEvent:from:withPasteboard:'; { available in 10_5 }
    function window_willUseFullScreenContentSize (window: NSWindow; proposedSize: NSSize): NSSize; message 'window:willUseFullScreenContentSize:'; { available in 10_7 }
    function window_willUseFullScreenPresentationOptions (window: NSWindow; proposedOptions: NSApplicationPresentationOptions): NSApplicationPresentationOptions; message 'window:willUseFullScreenPresentationOptions:'; { available in 10_7 }
    function customWindowsToEnterFullScreenForWindow (window: NSWindow): NSArray; message 'customWindowsToEnterFullScreenForWindow:'; { available in 10_7 }
    procedure window_startCustomAnimationToEnterFullScreenWithDuration (window: NSWindow; duration: NSTimeInterval); message 'window:startCustomAnimationToEnterFullScreenWithDuration:'; { available in 10_7 }
    procedure windowDidFailToEnterFullScreen (window: NSWindow); message 'windowDidFailToEnterFullScreen:'; { available in 10_7 }
    function customWindowsToExitFullScreenForWindow (window: NSWindow): NSArray; message 'customWindowsToExitFullScreenForWindow:'; { available in 10_7 }
    procedure window_startCustomAnimationToExitFullScreenWithDuration (window: NSWindow; duration: NSTimeInterval); message 'window:startCustomAnimationToExitFullScreenWithDuration:'; { available in 10_7 }
    function customWindowsToEnterFullScreenForWindow_onScreen (window: NSWindow; screen: NSScreen): NSArray; message 'customWindowsToEnterFullScreenForWindow:onScreen:'; { available in 10_9 }
    procedure window_startCustomAnimationToEnterFullScreenOnScreen_withDuration (window: NSWindow; screen: NSScreen; duration: NSTimeInterval); message 'window:startCustomAnimationToEnterFullScreenOnScreen:withDuration:'; { available in 10_9 }
    procedure windowDidFailToExitFullScreen (window: NSWindow); message 'windowDidFailToExitFullScreen:'; { available in 10_7 }
    function window_willResizeForVersionBrowserWithMaxPreferredSize_maxAllowedSize (window: NSWindow; maxPreferredFrameSize: NSSize; maxAllowedFrameSize: NSSize): NSSize; message 'window:willResizeForVersionBrowserWithMaxPreferredSize:maxAllowedSize:'; { available in 10_7 }
    procedure window_willEncodeRestorableState (window: NSWindow; state: NSCoder); message 'window:willEncodeRestorableState:'; { available in 10_7 }
    procedure window_didDecodeRestorableState (window: NSWindow; state: NSCoder); message 'window:didDecodeRestorableState:'; { available in 10_7 }
    procedure windowDidResize (notification: NSNotification); message 'windowDidResize:';
    procedure windowDidExpose (notification: NSNotification); message 'windowDidExpose:';
    procedure windowWillMove (notification: NSNotification); message 'windowWillMove:';
    procedure windowDidMove (notification: NSNotification); message 'windowDidMove:';
    procedure windowDidBecomeKey (notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey (notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidBecomeMain (notification: NSNotification); message 'windowDidBecomeMain:';
    procedure windowDidResignMain (notification: NSNotification); message 'windowDidResignMain:';
    procedure windowWillClose (notification: NSNotification); message 'windowWillClose:';
    procedure windowWillMiniaturize (notification: NSNotification); message 'windowWillMiniaturize:';
    procedure windowDidMiniaturize (notification: NSNotification); message 'windowDidMiniaturize:';
    procedure windowDidDeminiaturize (notification: NSNotification); message 'windowDidDeminiaturize:';
    procedure windowDidUpdate (notification: NSNotification); message 'windowDidUpdate:';
    procedure windowDidChangeScreen (notification: NSNotification); message 'windowDidChangeScreen:';
    procedure windowDidChangeScreenProfile (notification: NSNotification); message 'windowDidChangeScreenProfile:';
    procedure windowDidChangeBackingProperties (notification: NSNotification); message 'windowDidChangeBackingProperties:'; { available in 10_7 }
    procedure windowWillBeginSheet (notification: NSNotification); message 'windowWillBeginSheet:';
    procedure windowDidEndSheet (notification: NSNotification); message 'windowDidEndSheet:';
    procedure windowWillStartLiveResize (notification: NSNotification); message 'windowWillStartLiveResize:'; { available in 10_6 }
    procedure windowDidEndLiveResize (notification: NSNotification); message 'windowDidEndLiveResize:'; { available in 10_6 }
    procedure windowWillEnterFullScreen (notification: NSNotification); message 'windowWillEnterFullScreen:'; { available in 10_7 }
    procedure windowDidEnterFullScreen (notification: NSNotification); message 'windowDidEnterFullScreen:'; { available in 10_7 }
    procedure windowWillExitFullScreen (notification: NSNotification); message 'windowWillExitFullScreen:'; { available in 10_7 }
    procedure windowDidExitFullScreen (notification: NSNotification); message 'windowDidExitFullScreen:'; { available in 10_7 }
    procedure windowWillEnterVersionBrowser (notification: NSNotification); message 'windowWillEnterVersionBrowser:'; { available in 10_7 }
    procedure windowDidEnterVersionBrowser (notification: NSNotification); message 'windowDidEnterVersionBrowser:'; { available in 10_7 }
    procedure windowWillExitVersionBrowser (notification: NSNotification); message 'windowWillExitVersionBrowser:'; { available in 10_7 }
    procedure windowDidExitVersionBrowser (notification: NSNotification); message 'windowDidExitVersionBrowser:'; { available in 10_7 }
    procedure windowDidChangeOcclusionState (notification: NSNotification); message 'windowDidChangeOcclusionState:'; { available in 10_9 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSWindowDidBecomeKeyNotification: NSString; cvar; external;
  NSWindowDidBecomeMainNotification: NSString; cvar; external;
  NSWindowDidChangeScreenNotification: NSString; cvar; external;
  NSWindowDidDeminiaturizeNotification: NSString; cvar; external;
  NSWindowDidExposeNotification: NSString; cvar; external;
  NSWindowDidMiniaturizeNotification: NSString; cvar; external;
  NSWindowDidMoveNotification: NSString; cvar; external;
  NSWindowDidResignKeyNotification: NSString; cvar; external;
  NSWindowDidResignMainNotification: NSString; cvar; external;
  NSWindowDidResizeNotification: NSString; cvar; external;
  NSWindowDidUpdateNotification: NSString; cvar; external;
  NSWindowWillCloseNotification: NSString; cvar; external;
  NSWindowWillMiniaturizeNotification: NSString; cvar; external;
  NSWindowWillMoveNotification: NSString; cvar; external;
  NSWindowWillBeginSheetNotification: NSString; cvar; external;
  NSWindowDidEndSheetNotification: NSString; cvar; external;
  NSWindowDidChangeBackingPropertiesNotification: NSString { available in 10_7 }; cvar; external;
  NSBackingPropertyOldScaleFactorKey: NSString { available in 10_7 }; cvar; external;
  NSBackingPropertyOldColorSpaceKey: NSString { available in 10_7 }; cvar; external;
  NSWindowDidChangeScreenProfileNotification: NSString; cvar; external;
  NSWindowWillStartLiveResizeNotification: NSString { available in 10_6 }; cvar; external;
  NSWindowDidEndLiveResizeNotification: NSString { available in 10_6 }; cvar; external;
  NSWindowWillEnterFullScreenNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowDidEnterFullScreenNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowWillExitFullScreenNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowDidExitFullScreenNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowWillEnterVersionBrowserNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowDidEnterVersionBrowserNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowWillExitVersionBrowserNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowDidExitVersionBrowserNotification: NSString { available in 10_7 }; cvar; external;
  NSWindowDidChangeOcclusionStateNotification: NSString { available in 10_9 }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSUnscaledWindowMask = 1 shl 11;
{$endif}


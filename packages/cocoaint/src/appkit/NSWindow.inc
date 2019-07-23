{ Parsed from Appkit.framework NSWindow.h }

{$ifdef TYPES}
{$ifndef NSWINDOW_PAS_T}
{$define NSWINDOW_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithCustomSheetPosition = 686.0;
  NSAppKitVersionNumberWithDeferredWindowDisplaySupport = 1019.0;

{ Constants }

const
  NSBorderlessWindowMask = 0;
  NSTitledWindowMask = 1 shl 0;
  NSClosableWindowMask = 1 shl 1;
  NSMiniaturizableWindowMask = 1 shl 2;
  NSResizableWindowMask = 1 shl 3;

const
  NSTexturedBackgroundWindowMask = 1 shl 8;

const
  NSUnscaledWindowMask = 1 shl 11;

const
  NSUnifiedTitleAndToolbarWindowMask = 1 shl 12;

const
  NSDisplayWindowRunLoopOrdering = 600000;
  NSResetCursorRectsRunLoopOrdering = 700000;

const
  NSWindowSharingNone = 0;
  NSWindowSharingReadOnly = 1;
  NSWindowSharingReadWrite = 2;

const
  NSWindowBackingLocationDefault = 0;
  NSWindowBackingLocationVideoMemory = 1;
  NSWindowBackingLocationMainMemory = 2;

const
  NSWindowCollectionBehaviorDefault = 0;
  NSWindowCollectionBehaviorCanJoinAllSpaces = 1 shl 0;
  NSWindowCollectionBehaviorMoveToActiveSpace = 1 shl 1;

const
  NSWindowCollectionBehaviorManaged = 1 shl 2;
  NSWindowCollectionBehaviorTransient = 1 shl 3;
  NSWindowCollectionBehaviorStationary = 1 shl 4;

const
  NSWindowCollectionBehaviorParticipatesInCycle = 1 shl 5;
  NSWindowCollectionBehaviorIgnoresCycle = 1 shl 6;

const
  NSWindowNumberListAllApplications = 1 shl 0;
  NSWindowNumberListAllSpaces = 1 shl 4;

const
  NSDirectSelection = 0;
  NSSelectingNext = 1;
  NSSelectingPrevious = 2;

const
  NSWindowCloseButton = 0;
  NSWindowMiniaturizeButton = 1;
  NSWindowZoomButton = 2;
  NSWindowToolbarButton = 3;
  NSWindowDocumentIconButton = 4;

{ Types }
type
  NSWindowSharingType = NSUInteger;
  NSWindowSharingTypePtr = ^NSWindowSharingType;
  NSWindowBackingLocation = NSUInteger;
  NSWindowBackingLocationPtr = ^NSWindowBackingLocation;
  NSWindowCollectionBehavior = NSUInteger;
  NSWindowCollectionBehaviorPtr = ^NSWindowCollectionBehavior;
  NSWindowNumberListOptions = NSUInteger;
  NSWindowNumberListOptionsPtr = ^NSWindowNumberListOptions;
  NSSelectionDirection = NSUInteger;
  NSSelectionDirectionPtr = ^NSSelectionDirection;
  NSWindowButton = NSUInteger;
  NSWindowButtonPtr = ^NSWindowButton;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSWINDOW_PAS_R}
{$define NSWINDOW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSWINDOW_PAS_F}
{$define NSWINDOW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSWINDOW_PAS_S}
{$define NSWINDOW_PAS_S}

{ External string constants }
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
  NSWindowDidChangeScreenProfileNotification: NSString; cvar; external;
  NSWindowWillStartLiveResizeNotification: NSString; cvar; external;
  NSWindowDidEndLiveResizeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSWindowDelegateProtocol = objcprotocol;
  NSWindow = objcclass;
  NSWindowPointer = ^NSWindow;
  NSWindowPtr = NSWindowPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSWINDOW_PAS_C}
{$define NSWINDOW_PAS_C}

{ NSWindow }
  NSWindow = objcclass external (NSResponder)
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
    _cursorRects: Pointer;
    _trectTable: Pointer;
    _miniIcon: NSImage;
    _unused: cint;
    _dragTypes: NSMutableSet;
    _representedURL: NSURL;
    _sizeLimits: NSSizePtr;
    _frameSaveName: NSString;
    _regDragTypes: NSSet;
    __wFlags: record
      case byte of
       0: (_anonbitfield_NSWindow0: cuint);
       1: (data: bitpacked record
        backing: 0..((1 shl 2)-1);
        visible: 0..1;
        _isMainWindow: 0..1;
        _isKeyWindow: 0..1;
        _hidesOnDeactivate: 0..1;
        dontFreeWhenClosed: 0..1;
        oneShot: 0..1;
        deferred: 0..1;
        cursorRectsDisabled: 0..1;
        haveFreeCursorRects: 0..1;
        validCursorRects: 0..1;
        docEdited: 0..1;
        dynamicDepthLimit: 0..1;
        _worksWhenModal: 0..1;
        limitedBecomeKey: 0..1;
        needsFlush: 0..1;
        _viewsNeedDisplay: 0..1;
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
        _depthLimit: 0..((1 shl 4)-1);
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
        interfaceStyle: 0..((1 shl 3)-1);
        _keyViewSelectionDirection: 0..((1 shl 2)-1);
        defaultButtonCellKETemporarilyDisabled: 0..1;
        defaultButtonCellKEDisabled: 0..1;
        menuHasBeenSet: 0..1;
        wantsToBeModal: 0..1;
        showingModalFrame: 0..1;
        isTerminating: 0..1;
        win32MouseActivationInProgress: 0..1;
        makingFirstResponderForMouseDown: 0..1;
        needsZoom: 0..1;
        sentWindowNeedsDisplayMsg: 0..1;
        liveResizeActive: 0..1;
       end;
      );
      end;
    _defaultButtonCell: id;
    _initialFirstResponder: NSView;
    _auxiliaryStorage: NSWindowAuxiliary;
    
  public
    class function frameRectForContentRect_styleMask(cRect: NSRect; aStyle: NSUInteger): NSRect; message 'frameRectForContentRect:styleMask:';
    class function contentRectForFrameRect_styleMask(fRect: NSRect; aStyle: NSUInteger): NSRect; message 'contentRectForFrameRect:styleMask:';
    class function minFrameWidthWithTitle_styleMask(aTitle: NSString; aStyle: NSUInteger): CGFloat; message 'minFrameWidthWithTitle:styleMask:';
    class function defaultDepthLimit: NSWindowDepth; message 'defaultDepthLimit';
    function frameRectForContentRect(contentRect: NSRect): NSRect; message 'frameRectForContentRect:';
    function contentRectForFrameRect(frameRect: NSRect): NSRect; message 'contentRectForFrameRect:';
    function initWithContentRect_styleMask_backing_defer(contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBOOL): id; message 'initWithContentRect:styleMask:backing:defer:';
    function initWithContentRect_styleMask_backing_defer_screen(contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBOOL; screen_: NSScreen): id; message 'initWithContentRect:styleMask:backing:defer:screen:';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    procedure setRepresentedURL(url: NSURL); message 'setRepresentedURL:';
    function representedURL: NSURL; message 'representedURL';
    function representedFilename: NSString; message 'representedFilename';
    procedure setRepresentedFilename(aString: NSString); message 'setRepresentedFilename:';
    procedure setTitleWithRepresentedFilename(filename: NSString); message 'setTitleWithRepresentedFilename:';
    procedure setExcludedFromWindowsMenu(flag: ObjCBOOL); message 'setExcludedFromWindowsMenu:';
    function isExcludedFromWindowsMenu: ObjCBOOL; message 'isExcludedFromWindowsMenu';
    procedure setContentView(aView: NSView); message 'setContentView:';
    function contentView: id; message 'contentView';
    procedure setDelegate(anObject: NSWindowDelegateProtocol); message 'setDelegate:';
    function delegate: NSWindowDelegateProtocol; message 'delegate';
    function windowNumber: NSInteger; message 'windowNumber';
    function styleMask: NSUInteger; message 'styleMask';
    procedure setStyleMask(styleMask_: NSUInteger); message 'setStyleMask:';
    function fieldEditor_forObject(createFlag: ObjCBOOL; anObject: id): NSText; message 'fieldEditor:forObject:';
    procedure endEditingFor(anObject: id); message 'endEditingFor:';
    function constrainFrameRect_toScreen(frameRect: NSRect; screen_: NSScreen): NSRect; message 'constrainFrameRect:toScreen:';
    procedure setFrame_display(frameRect: NSRect; flag: ObjCBOOL); message 'setFrame:display:';
    procedure setContentSize(aSize: NSSize); message 'setContentSize:';
    procedure setFrameOrigin(aPoint: NSPoint); message 'setFrameOrigin:';
    procedure setFrameTopLeftPoint(aPoint: NSPoint); message 'setFrameTopLeftPoint:';
    function cascadeTopLeftFromPoint(topLeftPoint: NSPoint): NSPoint; message 'cascadeTopLeftFromPoint:';
    function frame: NSRect; message 'frame';
    function animationResizeTime(newFrame: NSRect): NSTimeInterval; message 'animationResizeTime:';
    procedure setFrame_display_animate(frameRect: NSRect; displayFlag: ObjCBOOL; animateFlag: ObjCBOOL); message 'setFrame:display:animate:';
    function inLiveResize: ObjCBOOL; message 'inLiveResize';
    procedure setShowsResizeIndicator(show: ObjCBOOL); message 'setShowsResizeIndicator:';
    function showsResizeIndicator: ObjCBOOL; message 'showsResizeIndicator';
    procedure setResizeIncrements(increments: NSSize); message 'setResizeIncrements:';
    function resizeIncrements: NSSize; message 'resizeIncrements';
    procedure setAspectRatio(ratio: NSSize); message 'setAspectRatio:';
    function aspectRatio: NSSize; message 'aspectRatio';
    procedure setContentResizeIncrements(increments: NSSize); message 'setContentResizeIncrements:';
    function contentResizeIncrements: NSSize; message 'contentResizeIncrements';
    procedure setContentAspectRatio(ratio: NSSize); message 'setContentAspectRatio:';
    function contentAspectRatio: NSSize; message 'contentAspectRatio';
    procedure useOptimizedDrawing(flag: ObjCBOOL); message 'useOptimizedDrawing:';
    procedure disableFlushWindow; message 'disableFlushWindow';
    procedure enableFlushWindow; message 'enableFlushWindow';
    function isFlushWindowDisabled: ObjCBOOL; message 'isFlushWindowDisabled';
    procedure flushWindow; message 'flushWindow';
    procedure flushWindowIfNeeded; message 'flushWindowIfNeeded';
    procedure setViewsNeedDisplay(flag: ObjCBOOL); message 'setViewsNeedDisplay:';
    function viewsNeedDisplay: ObjCBOOL; message 'viewsNeedDisplay';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure display; message 'display';
    procedure setAutodisplay(flag: ObjCBOOL); message 'setAutodisplay:';
    function isAutodisplay: ObjCBOOL; message 'isAutodisplay';
    function preservesContentDuringLiveResize: ObjCBOOL; message 'preservesContentDuringLiveResize';
    procedure setPreservesContentDuringLiveResize(flag: ObjCBOOL); message 'setPreservesContentDuringLiveResize:';
    procedure update; message 'update';
    function makeFirstResponder(aResponder: NSResponder): ObjCBOOL; message 'makeFirstResponder:';
    function firstResponder: NSResponder; message 'firstResponder';
    function resizeFlags: NSInteger; message 'resizeFlags';
    procedure keyDown(theEvent: NSEvent); message 'keyDown:';
    procedure close; message 'close';
    procedure setReleasedWhenClosed(flag: ObjCBOOL); message 'setReleasedWhenClosed:';
    function isReleasedWhenClosed: ObjCBOOL; message 'isReleasedWhenClosed';
    procedure miniaturize(sender: id); message 'miniaturize:';
    procedure deminiaturize(sender: id); message 'deminiaturize:';
    function isZoomed: ObjCBOOL; message 'isZoomed';
    procedure zoom(sender: id); message 'zoom:';
    function isMiniaturized: ObjCBOOL; message 'isMiniaturized';
    function tryToPerform_with(anAction: SEL; anObject: id): ObjCBOOL; message 'tryToPerform:with:';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setContentBorderThickness_forEdge(thickness: CGFloat; edge: NSRectEdge); message 'setContentBorderThickness:forEdge:';
    function contentBorderThicknessForEdge(edge: NSRectEdge): CGFloat; message 'contentBorderThicknessForEdge:';
    procedure setAutorecalculatesContentBorderThickness_forEdge(flag: ObjCBOOL; edge: NSRectEdge); message 'setAutorecalculatesContentBorderThickness:forEdge:';
    function autorecalculatesContentBorderThicknessForEdge(edge: NSRectEdge): ObjCBOOL; message 'autorecalculatesContentBorderThicknessForEdge:';
    procedure setMovable(flag: ObjCBOOL); message 'setMovable:';
    function isMovable: ObjCBOOL; message 'isMovable';
    procedure setMovableByWindowBackground(flag: ObjCBOOL); message 'setMovableByWindowBackground:';
    function isMovableByWindowBackground: ObjCBOOL; message 'isMovableByWindowBackground';
    procedure setHidesOnDeactivate(flag: ObjCBOOL); message 'setHidesOnDeactivate:';
    function hidesOnDeactivate: ObjCBOOL; message 'hidesOnDeactivate';
    procedure setCanHide(flag: ObjCBOOL); message 'setCanHide:';
    function canHide: ObjCBOOL; message 'canHide';
    procedure center; message 'center';
    procedure makeKeyAndOrderFront(sender: id); message 'makeKeyAndOrderFront:';
    procedure orderFront(sender: id); message 'orderFront:';
    procedure orderBack(sender: id); message 'orderBack:';
    procedure orderOut(sender: id); message 'orderOut:';
    procedure orderWindow_relativeTo(place: NSWindowOrderingMode; otherWin: NSInteger); message 'orderWindow:relativeTo:';
    procedure orderFrontRegardless; message 'orderFrontRegardless';
    procedure setMiniwindowImage(image: NSImage); message 'setMiniwindowImage:';
    procedure setMiniwindowTitle(title_: NSString); message 'setMiniwindowTitle:';
    function miniwindowImage: NSImage; message 'miniwindowImage';
    function miniwindowTitle: NSString; message 'miniwindowTitle';
    function dockTile: NSDockTile; message 'dockTile';
    procedure setDocumentEdited(flag: ObjCBOOL); message 'setDocumentEdited:';
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
    function preventsApplicationTerminationWhenModal: ObjCBOOL; message 'preventsApplicationTerminationWhenModal';
    procedure setPreventsApplicationTerminationWhenModal(flag: ObjCBOOL); message 'setPreventsApplicationTerminationWhenModal:';
    function convertBaseToScreen(aPoint: NSPoint): NSPoint; message 'convertBaseToScreen:';
    function convertScreenToBase(aPoint: NSPoint): NSPoint; message 'convertScreenToBase:';
    procedure performClose(sender: id); message 'performClose:';
    procedure performMiniaturize(sender: id); message 'performMiniaturize:';
    procedure performZoom(sender: id); message 'performZoom:';
    function gState: NSInteger; message 'gState';
    procedure setOneShot(flag: ObjCBOOL); message 'setOneShot:';
    function isOneShot: ObjCBOOL; message 'isOneShot';
    function dataWithEPSInsideRect(rect: NSRect): NSData; message 'dataWithEPSInsideRect:';
    function dataWithPDFInsideRect(rect: NSRect): NSData; message 'dataWithPDFInsideRect:';
    procedure print(sender: id); message 'print:';
    procedure disableCursorRects; message 'disableCursorRects';
    procedure enableCursorRects; message 'enableCursorRects';
    procedure discardCursorRects; message 'discardCursorRects';
    function areCursorRectsEnabled: ObjCBOOL; message 'areCursorRectsEnabled';
    procedure invalidateCursorRectsForView(aView: NSView); message 'invalidateCursorRectsForView:';
    procedure resetCursorRects; message 'resetCursorRects';
    procedure setAllowsToolTipsWhenApplicationIsInactive(allowWhenInactive: ObjCBOOL); message 'setAllowsToolTipsWhenApplicationIsInactive:';
    function allowsToolTipsWhenApplicationIsInactive: ObjCBOOL; message 'allowsToolTipsWhenApplicationIsInactive';
    procedure setBackingType(bufferingType: NSBackingStoreType); message 'setBackingType:';
    function backingType: NSBackingStoreType; message 'backingType';
    procedure setLevel(newLevel: NSInteger); message 'setLevel:';
    function level: NSInteger; message 'level';
    procedure setDepthLimit(limit: NSWindowDepth); message 'setDepthLimit:';
    function depthLimit: NSWindowDepth; message 'depthLimit';
    procedure setDynamicDepthLimit(flag: ObjCBOOL); message 'setDynamicDepthLimit:';
    function hasDynamicDepthLimit: ObjCBOOL; message 'hasDynamicDepthLimit';
    function screen: NSScreen; message 'screen';
    function deepestScreen: NSScreen; message 'deepestScreen';
    function canStoreColor: ObjCBOOL; message 'canStoreColor';
    procedure setHasShadow(hasShadow_: ObjCBOOL); message 'setHasShadow:';
    function hasShadow: ObjCBOOL; message 'hasShadow';
    procedure invalidateShadow; message 'invalidateShadow';
    procedure setAlphaValue(windowAlpha: CGFloat); message 'setAlphaValue:';
    function alphaValue: CGFloat; message 'alphaValue';
    procedure setOpaque(isOpaque_: ObjCBOOL); message 'setOpaque:';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setSharingType(type_: NSWindowSharingType); message 'setSharingType:';
    function sharingType: NSWindowSharingType; message 'sharingType';
    procedure setPreferredBackingLocation(backingLocation_: NSWindowBackingLocation); message 'setPreferredBackingLocation:';
    function preferredBackingLocation: NSWindowBackingLocation; message 'preferredBackingLocation';
    function backingLocation: NSWindowBackingLocation; message 'backingLocation';
    function allowsConcurrentViewDrawing: ObjCBOOL; message 'allowsConcurrentViewDrawing';
    procedure setAllowsConcurrentViewDrawing(flag: ObjCBOOL); message 'setAllowsConcurrentViewDrawing:';
    function displaysWhenScreenProfileChanges: ObjCBOOL; message 'displaysWhenScreenProfileChanges';
    procedure setDisplaysWhenScreenProfileChanges(flag: ObjCBOOL); message 'setDisplaysWhenScreenProfileChanges:';
    procedure disableScreenUpdatesUntilFlush; message 'disableScreenUpdatesUntilFlush';
    function canBecomeVisibleWithoutLogin: ObjCBOOL; message 'canBecomeVisibleWithoutLogin';
    procedure setCanBecomeVisibleWithoutLogin(flag: ObjCBOOL); message 'setCanBecomeVisibleWithoutLogin:';
    procedure setCollectionBehavior(behavior: NSWindowCollectionBehavior); message 'setCollectionBehavior:';
    function collectionBehavior: NSWindowCollectionBehavior; message 'collectionBehavior';
    function canBeVisibleOnAllSpaces: ObjCBOOL; message 'canBeVisibleOnAllSpaces'; deprecated 'in Mac OS X 10.5 and later';
    procedure setCanBeVisibleOnAllSpaces(flag: ObjCBOOL); message 'setCanBeVisibleOnAllSpaces:'; deprecated 'in Mac OS X 10.5 and later';
    function isOnActiveSpace: ObjCBOOL; message 'isOnActiveSpace';
    function stringWithSavedFrame: NSString; message 'stringWithSavedFrame';
    procedure setFrameFromString(string_: NSString); message 'setFrameFromString:';
    procedure saveFrameUsingName(name: NSString); message 'saveFrameUsingName:';
    function setFrameUsingName_force(name: NSString; force: ObjCBOOL): ObjCBOOL; message 'setFrameUsingName:force:';
    function setFrameUsingName(name: NSString): ObjCBOOL; message 'setFrameUsingName:';
    function setFrameAutosaveName(name: NSString): ObjCBOOL; message 'setFrameAutosaveName:';
    function frameAutosaveName: NSString; message 'frameAutosaveName';
    class procedure removeFrameUsingName(name: NSString); message 'removeFrameUsingName:';
    procedure cacheImageInRect(aRect: NSRect); message 'cacheImageInRect:';
    procedure restoreCachedImage; message 'restoreCachedImage';
    procedure discardCachedImage; message 'discardCachedImage';
    function minSize: NSSize; message 'minSize';
    function maxSize: NSSize; message 'maxSize';
    procedure setMinSize(size: NSSize); message 'setMinSize:';
    procedure setMaxSize(size: NSSize); message 'setMaxSize:';
    function contentMinSize: NSSize; message 'contentMinSize';
    function contentMaxSize: NSSize; message 'contentMaxSize';
    procedure setContentMinSize(size: NSSize); message 'setContentMinSize:';
    procedure setContentMaxSize(size: NSSize); message 'setContentMaxSize:';
    function nextEventMatchingMask(mask: NSUInteger): NSEvent; message 'nextEventMatchingMask:';
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: ObjCBOOL): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure discardEventsMatchingMask_beforeEvent(mask: NSUInteger; lastEvent: NSEvent); message 'discardEventsMatchingMask:beforeEvent:';
    procedure postEvent_atStart(event: NSEvent; flag: ObjCBOOL); message 'postEvent:atStart:';
    function currentEvent: NSEvent; message 'currentEvent';
    procedure setAcceptsMouseMovedEvents(flag: ObjCBOOL); message 'setAcceptsMouseMovedEvents:';
    function acceptsMouseMovedEvents: ObjCBOOL; message 'acceptsMouseMovedEvents';
    procedure setIgnoresMouseEvents(flag: ObjCBOOL); message 'setIgnoresMouseEvents:';
    function ignoresMouseEvents: ObjCBOOL; message 'ignoresMouseEvents';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    procedure sendEvent(theEvent: NSEvent); message 'sendEvent:';
    function mouseLocationOutsideOfEventStream: NSPoint; message 'mouseLocationOutsideOfEventStream';
    class procedure menuChanged(menu_: NSMenu); message 'menuChanged:';
    function windowController: id; message 'windowController';
    procedure setWindowController(windowController_: NSWindowController); message 'setWindowController:';
    function isSheet: ObjCBOOL; message 'isSheet';
    function attachedSheet: NSWindow; message 'attachedSheet';
    class function standardWindowButton_forStyleMask(b: NSWindowButton; styleMask_: NSUInteger): NSButton; message 'standardWindowButton:forStyleMask:';
    function standardWindowButton(b: NSWindowButton): NSButton; message 'standardWindowButton:';
    procedure addChildWindow_ordered(childWin: NSWindow; place: NSWindowOrderingMode); message 'addChildWindow:ordered:';
    procedure removeChildWindow(childWin: NSWindow); message 'removeChildWindow:';
    function childWindows: NSArray; message 'childWindows';
    function parentWindow: NSWindow; message 'parentWindow';
    procedure setParentWindow(window: NSWindow); message 'setParentWindow:';
    function graphicsContext: NSGraphicsContext; message 'graphicsContext';
    function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor';
    procedure setColorSpace(colorSpace_: NSColorSpace); message 'setColorSpace:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    class function windowNumbersWithOptions(options: NSWindowNumberListOptions): NSArray; message 'windowNumbersWithOptions:';
    class function windowNumberAtPoint_belowWindowWithWindowNumber(point: NSPoint; windowNumber_: NSInteger): NSInteger; message 'windowNumberAtPoint:belowWindowWithWindowNumber:';
  end;

{ NSKeyboardUI_NSWindowCategory }
  NSKeyboardUI_NSWindowCategory = objccategory external name 'NSKeyboardUI' (NSWindow)
    procedure setInitialFirstResponder(view: NSView); message 'setInitialFirstResponder:';
    function initialFirstResponder: NSView; message 'initialFirstResponder';
    procedure selectNextKeyView(sender: id); message 'selectNextKeyView:';
    procedure selectPreviousKeyView(sender: id); message 'selectPreviousKeyView:';
    procedure selectKeyViewFollowingView(aView: NSView); message 'selectKeyViewFollowingView:';
    procedure selectKeyViewPrecedingView(aView: NSView); message 'selectKeyViewPrecedingView:';
    function keyViewSelectionDirection: NSSelectionDirection; message 'keyViewSelectionDirection';
    procedure setDefaultButtonCell(defButt: NSButtonCell); message 'setDefaultButtonCell:';
    function defaultButtonCell: NSButtonCell; message 'defaultButtonCell';
    procedure disableKeyEquivalentForDefaultButtonCell; message 'disableKeyEquivalentForDefaultButtonCell';
    procedure enableKeyEquivalentForDefaultButtonCell; message 'enableKeyEquivalentForDefaultButtonCell';
    procedure setAutorecalculatesKeyViewLoop(flag: ObjCBOOL); message 'setAutorecalculatesKeyViewLoop:';
    function autorecalculatesKeyViewLoop: ObjCBOOL; message 'autorecalculatesKeyViewLoop';
    procedure recalculateKeyViewLoop; message 'recalculateKeyViewLoop';
  end;

{ NSToolbarSupportCategory }
  NSToolbarSupportCategory = objccategory external (NSWindow)
    procedure setToolbar(toolbar_: NSToolbar); message 'setToolbar:';
    function toolbar: NSToolbar; message 'toolbar';
    procedure toggleToolbarShown(sender: id); message 'toggleToolbarShown:';
    procedure runToolbarCustomizationPalette(sender: id); message 'runToolbarCustomizationPalette:';
    procedure setShowsToolbarButton(show: ObjCBOOL); message 'setShowsToolbarButton:';
    function showsToolbarButton: ObjCBOOL; message 'showsToolbarButton';
  end;

{ NSDrag_NSWindowCategory }
  NSDrag_NSWindowCategory = objccategory external name 'NSDrag' (NSWindow)
    procedure dragImage_at_offset_event_pasteboard_source_slideBack(anImage: NSImage; baseLocation: NSPoint; initialOffset: NSSize; event: NSEvent; pboard: NSPasteboard; sourceObj: id; slideFlag: ObjCBOOL); message 'dragImage:at:offset:event:pasteboard:source:slideBack:';
    procedure registerForDraggedTypes(newTypes: NSArray); message 'registerForDraggedTypes:';
    procedure unregisterDraggedTypes; message 'unregisterDraggedTypes';
  end;

{ NSCarbonExtensionsCategory }
  NSCarbonExtensionsCategory = objccategory external (NSWindow)
    function initWithWindowRef(windowRef_: Pointer): NSWindow; message 'initWithWindowRef:';
    function windowRef: Pointer; message 'windowRef';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSWINDOW_PAS_P}
{$define NSWINDOW_PAS_P}
  
{ NSWindowDelegate Protocol }
  NSWindowDelegateProtocol = objcprotocol external name 'NSWindowDelegate'
  optional
    function windowShouldClose(sender: id): ObjCBOOL; message 'windowShouldClose:';
    function windowWillReturnFieldEditor_toObject(sender: NSWindow; client: id): id; message 'windowWillReturnFieldEditor:toObject:';
    function windowWillResize_toSize(sender: NSWindow; frameSize: NSSize): NSSize; message 'windowWillResize:toSize:';
    function windowWillUseStandardFrame_defaultFrame(window: NSWindow; newFrame: NSRect): NSRect; message 'windowWillUseStandardFrame:defaultFrame:';
    function windowShouldZoom_toFrame(window: NSWindow; newFrame: NSRect): ObjCBOOL; message 'windowShouldZoom:toFrame:';
    function windowWillReturnUndoManager(window: NSWindow): NSUndoManager; message 'windowWillReturnUndoManager:';
    function window_willPositionSheet_usingRect(window: NSWindow; sheet: NSWindow; rect: NSRect): NSRect; message 'window:willPositionSheet:usingRect:';
    function window_shouldPopUpDocumentPathMenu(window: NSWindow; menu: NSMenu): ObjCBOOL; message 'window:shouldPopUpDocumentPathMenu:';
    function window_shouldDragDocumentWithEvent_from_withPasteboard(window: NSWindow; event: NSEvent; dragImageLocation: NSPoint; pasteboard: NSPasteboard): ObjCBOOL; message 'window:shouldDragDocumentWithEvent:from:withPasteboard:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidExpose(notification: NSNotification); message 'windowDidExpose:';
    procedure windowWillMove(notification: NSNotification); message 'windowWillMove:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidBecomeMain(notification: NSNotification); message 'windowDidBecomeMain:';
    procedure windowDidResignMain(notification: NSNotification); message 'windowDidResignMain:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowWillMiniaturize(notification: NSNotification); message 'windowWillMiniaturize:';
    procedure windowDidMiniaturize(notification: NSNotification); message 'windowDidMiniaturize:';
    procedure windowDidDeminiaturize(notification: NSNotification); message 'windowDidDeminiaturize:';
    procedure windowDidUpdate(notification: NSNotification); message 'windowDidUpdate:';
    procedure windowDidChangeScreen(notification: NSNotification); message 'windowDidChangeScreen:';
    procedure windowDidChangeScreenProfile(notification: NSNotification); message 'windowDidChangeScreenProfile:';
    procedure windowWillBeginSheet(notification: NSNotification); message 'windowWillBeginSheet:';
    procedure windowDidEndSheet(notification: NSNotification); message 'windowDidEndSheet:';
    procedure windowWillStartLiveResize(notification: NSNotification); message 'windowWillStartLiveResize:';
    procedure windowDidEndLiveResize(notification: NSNotification); message 'windowDidEndLiveResize:';
  end;
{$endif}
{$endif}

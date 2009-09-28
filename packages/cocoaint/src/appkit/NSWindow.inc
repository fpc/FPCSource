{ Parsed from Appkit.framework NSWindow.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSWINDOW_PAS_H}
{$define NSWINDOW_PAS_H}
type
  NSWindowPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSWINDOW_PAS_T}
{$define NSWINDOW_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithCustomSheetPosition = 686.0;

{ Constants }

const
  NSBorderlessWindowMask = 0;
  NSTitledWindowMask = 1  shl  0;
  NSClosableWindowMask = 1  shl  1;
  NSMiniaturizableWindowMask = 1  shl  2;
  NSResizableWindowMask = 1  shl  3;

const
  NSTexturedBackgroundWindowMask = 1  shl  8;

const
  NSUnscaledWindowMask = 1  shl  11;

const
  NSUnifiedTitleAndToolbarWindowMask = 1  shl  12;

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
  NSWindowCollectionBehaviorCanJoinAllSpaces = 1  shl  0;
  NSWindowCollectionBehaviorMoveToActiveSpace = 1  shl  1;

const
  NSDirectSelection = 0;
  NSSelectingNext = 0;
  NSSelectingPrevious = 1;

const
  NSWindowCloseButton = 0;
  NSWindowMiniaturizeButton = 1;
  NSWindowZoomButton = 2;
  NSWindowToolbarButton = 3;
  NSWindowDocumentIconButton = 4;

{ Types }
type
  NSWindowSharingType = NSUInteger;
  NSWindowBackingLocation = NSUInteger;
  NSWindowCollectionBehavior = NSUInteger;
  NSSelectionDirection = NSUInteger;
  NSWindowButton = NSUInteger;

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

{$endif}
{$endif}

{$ifdef FORWARD}
  NSWindow = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSWINDOW_PAS_C}
{$define NSWINDOW_PAS_C}

{ NSWindow }
  NSWindow = objcclass(NSResponder)
  private
    __frame: NSRect;
    __contentView: id;
    __delegate: id;
    __firstResponder: NSResponder;
    __lastLeftHit: NSView;
    __lastRightHit: NSView;
    __counterpart: id;
    __fieldEditor: id;
    __winEventMask: cint;
    __windowNum: NSInteger;
    __level: cint;
    __backgroundColor: NSColor;
    __borderView: id;
    __postingDisabled: char;
    __styleMask: char;
    __flushDisabled: char;
    __reservedWindow1: char;
    __cursorRects: Pointer;
    __trectTable: Pointer;
    __miniIcon: NSImage;
    __unused: cint;
    __dragTypes: NSMutableSet;
    __representedURL: NSURL;
    __sizeLimits: NSSize;
    __frameSaveName: NSString;
    __regDragTypes: NSSet;
    __wFlags: bitpacked record
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
        interfaceStyle: 0..((1 shl 3)-1);
        keyViewSelectionDirection: 0..((1 shl 2)-1);
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
    __defaultButtonCell: id;
    __initialFirstResponder: NSView;
    __auxiliaryStorage: NSWindowAuxiliary;
    
  public
    class function alloc: NSWindow; message 'alloc';

    class function frameRectForContentRect_styleMask(cRect: NSRect; aStyle: NSUInteger): NSRect; message 'frameRectForContentRect:styleMask:';
    class function contentRectForFrameRect_styleMask(fRect: NSRect; aStyle: NSUInteger): NSRect; message 'contentRectForFrameRect:styleMask:';
    class function minFrameWidthWithTitle_styleMask(aTitle: NSString; aStyle: NSUInteger): CGFloat; message 'minFrameWidthWithTitle:styleMask:';
    class function defaultDepthLimit: NSWindowDepth; message 'defaultDepthLimit';
    function frameRectForContentRect(contentRect: NSRect): NSRect; message 'frameRectForContentRect:';
    function contentRectForFrameRect(frameRect: NSRect): NSRect; message 'contentRectForFrameRect:';
    function initWithContentRect_styleMask_backing_defer(contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: Boolean): id; message 'initWithContentRect:styleMask:backing:defer:';
    function initWithContentRect_styleMask_backing_defer_screen(contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: Boolean; screen_: NSScreen): id; message 'initWithContentRect:styleMask:backing:defer:screen:';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    procedure setRepresentedURL(url: NSURL); message 'setRepresentedURL:';
    function representedURL: NSURL; message 'representedURL';
    function representedFilename: NSString; message 'representedFilename';
    procedure setRepresentedFilename(aString: NSString); message 'setRepresentedFilename:';
    procedure setTitleWithRepresentedFilename(filename: NSString); message 'setTitleWithRepresentedFilename:';
    procedure setExcludedFromWindowsMenu(flag: Boolean); message 'setExcludedFromWindowsMenu:';
    function isExcludedFromWindowsMenu: Boolean; message 'isExcludedFromWindowsMenu';
    procedure setContentView(aView: NSView); message 'setContentView:';
    function contentView: id; message 'contentView';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function windowNumber: NSInteger; message 'windowNumber';
    function styleMask: NSUInteger; message 'styleMask';
    function fieldEditor_forObject(createFlag: Boolean; anObject: id): NSText; message 'fieldEditor:forObject:';
    procedure endEditingFor(anObject: id); message 'endEditingFor:';
    function constrainFrameRect_toScreen(frameRect: NSRect; screen_: NSScreen): NSRect; message 'constrainFrameRect:toScreen:';
    procedure setFrame_display(frameRect: NSRect; flag: Boolean); message 'setFrame:display:';
    procedure setContentSize(aSize: NSSize); message 'setContentSize:';
    procedure setFrameOrigin(aPoint: NSPoint); message 'setFrameOrigin:';
    procedure setFrameTopLeftPoint(aPoint: NSPoint); message 'setFrameTopLeftPoint:';
    function cascadeTopLeftFromPoint(topLeftPoint: NSPoint): NSPoint; message 'cascadeTopLeftFromPoint:';
    function frame: NSRect; message 'frame';
    function animationResizeTime(newFrame: NSRect): NSTimeInterval; message 'animationResizeTime:';
    procedure setFrame_display_animate(frameRect: NSRect; displayFlag: Boolean; animateFlag: Boolean); message 'setFrame:display:animate:';
    procedure setShowsResizeIndicator(show: Boolean); message 'setShowsResizeIndicator:';
    function showsResizeIndicator: Boolean; message 'showsResizeIndicator';
    procedure setResizeIncrements(increments: NSSize); message 'setResizeIncrements:';
    function resizeIncrements: NSSize; message 'resizeIncrements';
    procedure setAspectRatio(ratio: NSSize); message 'setAspectRatio:';
    function aspectRatio: NSSize; message 'aspectRatio';
    procedure setContentResizeIncrements(increments: NSSize); message 'setContentResizeIncrements:';
    function contentResizeIncrements: NSSize; message 'contentResizeIncrements';
    procedure setContentAspectRatio(ratio: NSSize); message 'setContentAspectRatio:';
    function contentAspectRatio: NSSize; message 'contentAspectRatio';
    procedure useOptimizedDrawing(flag: Boolean); message 'useOptimizedDrawing:';
    procedure disableFlushWindow; message 'disableFlushWindow';
    procedure enableFlushWindow; message 'enableFlushWindow';
    function isFlushWindowDisabled: Boolean; message 'isFlushWindowDisabled';
    procedure flushWindow; message 'flushWindow';
    procedure flushWindowIfNeeded; message 'flushWindowIfNeeded';
    procedure setViewsNeedDisplay(flag: Boolean); message 'setViewsNeedDisplay:';
    function viewsNeedDisplay: Boolean; message 'viewsNeedDisplay';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure display; message 'display';
    procedure setAutodisplay(flag: Boolean); message 'setAutodisplay:';
    function isAutodisplay: Boolean; message 'isAutodisplay';
    function preservesContentDuringLiveResize: Boolean; message 'preservesContentDuringLiveResize';
    procedure setPreservesContentDuringLiveResize(flag: Boolean); message 'setPreservesContentDuringLiveResize:';
    procedure update; message 'update';
    function makeFirstResponder(aResponder: NSResponder): Boolean; message 'makeFirstResponder:';
    function firstResponder: NSResponder; message 'firstResponder';
    function resizeFlags: NSInteger; message 'resizeFlags';
    procedure keyDown(theEvent: NSEvent); message 'keyDown:';
    procedure close; message 'close';
    procedure setReleasedWhenClosed(flag: Boolean); message 'setReleasedWhenClosed:';
    function isReleasedWhenClosed: Boolean; message 'isReleasedWhenClosed';
    procedure miniaturize(sender: id); message 'miniaturize:';
    procedure deminiaturize(sender: id); message 'deminiaturize:';
    function isZoomed: Boolean; message 'isZoomed';
    procedure zoom(sender: id); message 'zoom:';
    function isMiniaturized: Boolean; message 'isMiniaturized';
    function tryToPerform_with(anAction: SEL; anObject: id): Boolean; message 'tryToPerform:with:';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setContentBorderThickness_forEdge(thickness: CGFloat; edge: NSRectEdge); message 'setContentBorderThickness:forEdge:';
    function contentBorderThicknessForEdge(edge: NSRectEdge): CGFloat; message 'contentBorderThicknessForEdge:';
    procedure setAutorecalculatesContentBorderThickness_forEdge(flag: Boolean; edge: NSRectEdge); message 'setAutorecalculatesContentBorderThickness:forEdge:';
    function autorecalculatesContentBorderThicknessForEdge(edge: NSRectEdge): Boolean; message 'autorecalculatesContentBorderThicknessForEdge:';
    procedure setMovableByWindowBackground(flag: Boolean); message 'setMovableByWindowBackground:';
    function isMovableByWindowBackground: Boolean; message 'isMovableByWindowBackground';
    procedure setHidesOnDeactivate(flag: Boolean); message 'setHidesOnDeactivate:';
    function hidesOnDeactivate: Boolean; message 'hidesOnDeactivate';
    procedure setCanHide(flag: Boolean); message 'setCanHide:';
    function canHide: Boolean; message 'canHide';
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
    procedure setDocumentEdited(flag: Boolean); message 'setDocumentEdited:';
    function isDocumentEdited: Boolean; message 'isDocumentEdited';
    function isVisible: Boolean; message 'isVisible';
    function isKeyWindow: Boolean; message 'isKeyWindow';
    function isMainWindow: Boolean; message 'isMainWindow';
    function canBecomeKeyWindow: Boolean; message 'canBecomeKeyWindow';
    function canBecomeMainWindow: Boolean; message 'canBecomeMainWindow';
    procedure makeKeyWindow; message 'makeKeyWindow';
    procedure makeMainWindow; message 'makeMainWindow';
    procedure becomeKeyWindow; message 'becomeKeyWindow';
    procedure resignKeyWindow; message 'resignKeyWindow';
    procedure becomeMainWindow; message 'becomeMainWindow';
    procedure resignMainWindow; message 'resignMainWindow';
    function worksWhenModal: Boolean; message 'worksWhenModal';
    function convertBaseToScreen(aPoint: NSPoint): NSPoint; message 'convertBaseToScreen:';
    function convertScreenToBase(aPoint: NSPoint): NSPoint; message 'convertScreenToBase:';
    procedure performClose(sender: id); message 'performClose:';
    procedure performMiniaturize(sender: id); message 'performMiniaturize:';
    procedure performZoom(sender: id); message 'performZoom:';
    function gState: NSInteger; message 'gState';
    procedure setOneShot(flag: Boolean); message 'setOneShot:';
    function isOneShot: Boolean; message 'isOneShot';
    function dataWithEPSInsideRect(rect: NSRect): NSData; message 'dataWithEPSInsideRect:';
    function dataWithPDFInsideRect(rect: NSRect): NSData; message 'dataWithPDFInsideRect:';
    procedure print(sender: id); message 'print:';
    procedure disableCursorRects; message 'disableCursorRects';
    procedure enableCursorRects; message 'enableCursorRects';
    procedure discardCursorRects; message 'discardCursorRects';
    function areCursorRectsEnabled: Boolean; message 'areCursorRectsEnabled';
    procedure invalidateCursorRectsForView(aView: NSView); message 'invalidateCursorRectsForView:';
    procedure resetCursorRects; message 'resetCursorRects';
    procedure setAllowsToolTipsWhenApplicationIsInactive(allowWhenInactive: Boolean); message 'setAllowsToolTipsWhenApplicationIsInactive:';
    function allowsToolTipsWhenApplicationIsInactive: Boolean; message 'allowsToolTipsWhenApplicationIsInactive';
    procedure setBackingType(bufferingType: NSBackingStoreType); message 'setBackingType:';
    function backingType: NSBackingStoreType; message 'backingType';
    procedure setLevel(newLevel: NSInteger); message 'setLevel:';
    function level: NSInteger; message 'level';
    procedure setDepthLimit(limit: NSWindowDepth); message 'setDepthLimit:';
    function depthLimit: NSWindowDepth; message 'depthLimit';
    procedure setDynamicDepthLimit(flag: Boolean); message 'setDynamicDepthLimit:';
    function hasDynamicDepthLimit: Boolean; message 'hasDynamicDepthLimit';
    function screen: NSScreen; message 'screen';
    function deepestScreen: NSScreen; message 'deepestScreen';
    function canStoreColor: Boolean; message 'canStoreColor';
    procedure setHasShadow(hasShadow_: Boolean); message 'setHasShadow:';
    function hasShadow: Boolean; message 'hasShadow';
    procedure invalidateShadow; message 'invalidateShadow';
    procedure setAlphaValue(windowAlpha: CGFloat); message 'setAlphaValue:';
    function alphaValue: CGFloat; message 'alphaValue';
    procedure setOpaque(isOpaque_: Boolean); message 'setOpaque:';
    function isOpaque: Boolean; message 'isOpaque';
    procedure setSharingType(type_: NSWindowSharingType); message 'setSharingType:';
    function sharingType: NSWindowSharingType; message 'sharingType';
    procedure setPreferredBackingLocation(backingLocation_: NSWindowBackingLocation); message 'setPreferredBackingLocation:';
    function preferredBackingLocation: NSWindowBackingLocation; message 'preferredBackingLocation';
    function backingLocation: NSWindowBackingLocation; message 'backingLocation';
    function displaysWhenScreenProfileChanges: Boolean; message 'displaysWhenScreenProfileChanges';
    procedure setDisplaysWhenScreenProfileChanges(flag: Boolean); message 'setDisplaysWhenScreenProfileChanges:';
    procedure disableScreenUpdatesUntilFlush; message 'disableScreenUpdatesUntilFlush';
    function canBecomeVisibleWithoutLogin: Boolean; message 'canBecomeVisibleWithoutLogin';
    procedure setCanBecomeVisibleWithoutLogin(flag: Boolean); message 'setCanBecomeVisibleWithoutLogin:';
    procedure setCollectionBehavior(behavior: NSWindowCollectionBehavior); message 'setCollectionBehavior:';
    function collectionBehavior: NSWindowCollectionBehavior; message 'collectionBehavior';
    procedure setCanBeVisibleOnAllSpaces(flag: Boolean); message 'setCanBeVisibleOnAllSpaces:';
    function stringWithSavedFrame: NSString; message 'stringWithSavedFrame';
    procedure setFrameFromString(string_: NSString); message 'setFrameFromString:';
    procedure saveFrameUsingName(name: NSString); message 'saveFrameUsingName:';
    function setFrameUsingName_force(name: NSString; force: Boolean): Boolean; message 'setFrameUsingName:force:';
    function setFrameUsingName(name: NSString): Boolean; message 'setFrameUsingName:';
    function setFrameAutosaveName(name: NSString): Boolean; message 'setFrameAutosaveName:';
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
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: Boolean): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure discardEventsMatchingMask_beforeEvent(mask: NSUInteger; lastEvent: NSEvent); message 'discardEventsMatchingMask:beforeEvent:';
    procedure postEvent_atStart(event: NSEvent; flag: Boolean); message 'postEvent:atStart:';
    function currentEvent: NSEvent; message 'currentEvent';
    procedure setAcceptsMouseMovedEvents(flag: Boolean); message 'setAcceptsMouseMovedEvents:';
    function acceptsMouseMovedEvents: Boolean; message 'acceptsMouseMovedEvents';
    procedure setIgnoresMouseEvents(flag: Boolean); message 'setIgnoresMouseEvents:';
    function ignoresMouseEvents: Boolean; message 'ignoresMouseEvents';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    procedure sendEvent(theEvent: NSEvent); message 'sendEvent:';
    function mouseLocationOutsideOfEventStream: NSPoint; message 'mouseLocationOutsideOfEventStream';
    class procedure menuChanged(menu_: NSMenu); message 'menuChanged:';
    function windowController: id; message 'windowController';
    procedure setWindowController(windowController_: NSWindowController); message 'setWindowController:';
    function isSheet: Boolean; message 'isSheet';
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

    { Category: NSKeyboardUI }
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
    procedure setAutorecalculatesKeyViewLoop(flag: Boolean); message 'setAutorecalculatesKeyViewLoop:';
    function autorecalculatesKeyViewLoop: Boolean; message 'autorecalculatesKeyViewLoop';
    procedure recalculateKeyViewLoop; message 'recalculateKeyViewLoop';

    { Category: NSToolbarSupport }
    procedure setToolbar(toolbar_: NSToolbar); message 'setToolbar:';
    function toolbar: NSToolbar; message 'toolbar';
    procedure toggleToolbarShown(sender: id); message 'toggleToolbarShown:';
    procedure runToolbarCustomizationPalette(sender: id); message 'runToolbarCustomizationPalette:';
    procedure setShowsToolbarButton(show: Boolean); message 'setShowsToolbarButton:';
    function showsToolbarButton: Boolean; message 'showsToolbarButton';

    { Category: NSDrag }
    procedure dragImage_at_offset_event_pasteboard_source_slideBack(anImage: NSImage; baseLocation: NSPoint; initialOffset: NSSize; event: NSEvent; pboard: NSPasteboard; sourceObj: id; slideFlag: Boolean); message 'dragImage:at:offset:event:pasteboard:source:slideBack:';
    procedure registerForDraggedTypes(newTypes: NSArray); message 'registerForDraggedTypes:';
    procedure unregisterDraggedTypes; message 'unregisterDraggedTypes';

    { Category: NSCarbonExtensions }
    function initWithWindowRef(windowRef: Pointer): NSWindow; message 'initWithWindowRef:';
    function windowRef: Pointer; message 'windowRef';

    { Category: Drawers }
    function drawers: NSArray; message 'drawers';

    { Category: NSScripting }
    function hasCloseBox: Boolean; message 'hasCloseBox';
    function hasTitleBar: Boolean; message 'hasTitleBar';
    function isFloatingPanel: Boolean; message 'isFloatingPanel';
    function isMiniaturizable: Boolean; message 'isMiniaturizable';
    function isModalPanel: Boolean; message 'isModalPanel';
    function isResizable: Boolean; message 'isResizable';
    function isZoomable: Boolean; message 'isZoomable';
    function orderedIndex: NSInteger; message 'orderedIndex';
    procedure setIsMiniaturized(flag: Boolean); message 'setIsMiniaturized:';
    procedure setIsVisible(flag: Boolean); message 'setIsVisible:';
    procedure setIsZoomed(flag: Boolean); message 'setIsZoomed:';
    procedure setOrderedIndex(index: NSInteger); message 'setOrderedIndex:';
    function handleCloseScriptCommand(command: NSCloseCommand): id; message 'handleCloseScriptCommand:';
    function handlePrintScriptCommand(command: NSScriptCommand): id; message 'handlePrintScriptCommand:';
    function handleSaveScriptCommand(command: NSScriptCommand): id; message 'handleSaveScriptCommand:';
  end; external;

{$endif}
{$endif}

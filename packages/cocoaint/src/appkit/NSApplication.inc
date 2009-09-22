{ Parsed from Appkit.framework NSApplication.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSAPPLICATION_PAS_H}
{$define NSAPPLICATION_PAS_H}
type
  NSApplicationPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSAPPLICATION_PAS_T}
{$define NSAPPLICATION_PAS_T}

{ Defines }
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

{ Constants }

const
  NSUpdateWindowsRunLoopOrdering = 500000;

const
  NSCriticalRequest = 0;
  NSInformationalRequest = 10;

const
  NSApplicationDelegateReplySuccess = 0;
  NSApplicationDelegateReplyCancel = 1;
  NSApplicationDelegateReplyFailure = 2;

const
  NSTerminateCancel = 0;
  NSTerminateNow = 1;
  NSTerminateLater = 2;

const
  NSPrintingCancelled = 0;
  NSPrintingSuccess = 1;
  NSPrintingFailure = 3;
  NSPrintingReplyLater = 2;

{ Types }
type
  NSModalSession = Pointer;
  NSThreadPrivate = Pointer;
  _NSThreadPrivate = NSThreadPrivate;
  NSRequestUserAttentionType = NSUInteger;
  NSApplicationDelegateReply = NSUInteger;
  NSApplicationTerminateReply = NSUInteger;
  NSApplicationPrintReply = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAPPLICATION_PAS_R}
{$define NSAPPLICATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAPPLICATION_PAS_F}
{$define NSAPPLICATION_PAS_F}

{ Functions }
function NSApplicationMain(argc: cint; argvPointer: Pointer {array of char}): cint; cdecl; external name 'NSApplicationMain';
function NSApplicationLoad: Boolean; cdecl; external name 'NSApplicationLoad';
function NSShowsServicesMenuItem(Pointer_: NSString): Boolean; cdecl; external name 'NSShowsServicesMenuItem';
function NSSetShowsServicesMenuItem(Pointer_: NSString; enabled: Boolean): NSInteger; cdecl; external name 'NSSetShowsServicesMenuItem';
procedure NSUpdateDynamicServices; cdecl; external name 'NSUpdateDynamicServices';
function NSPerformService(itemNamePointer: NSString; pboardPointer: NSPasteboard): Boolean; cdecl; external name 'NSPerformService';
procedure NSRegisterServicesProvider(provider: id; namePointer: NSString); cdecl; external name 'NSRegisterServicesProvider';
procedure NSUnregisterServicesProvider(namePointer: NSString); cdecl; external name 'NSUnregisterServicesProvider';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAPPLICATION_PAS_T}
{$define NSAPPLICATION_PAS_T}

{ External symbols }
var
  NSAppKitVersionNumber: double; external name '_NSAppKitVersionNumber';

{ External string constants }
var
  NSModalPanelRunLoopMode: NSString; external name '_NSModalPanelRunLoopMode';
  NSEventTrackingRunLoopMode: NSString; external name '_NSEventTrackingRunLoopMode';
  NSApplicationDidBecomeActiveNotification: NSString; external name '_NSApplicationDidBecomeActiveNotification';
  NSApplicationDidHideNotification: NSString; external name '_NSApplicationDidHideNotification';
  NSApplicationDidFinishLaunchingNotification: NSString; external name '_NSApplicationDidFinishLaunchingNotification';
  NSApplicationDidResignActiveNotification: NSString; external name '_NSApplicationDidResignActiveNotification';
  NSApplicationDidUnhideNotification: NSString; external name '_NSApplicationDidUnhideNotification';
  NSApplicationDidUpdateNotification: NSString; external name '_NSApplicationDidUpdateNotification';
  NSApplicationWillBecomeActiveNotification: NSString; external name '_NSApplicationWillBecomeActiveNotification';
  NSApplicationWillHideNotification: NSString; external name '_NSApplicationWillHideNotification';
  NSApplicationWillFinishLaunchingNotification: NSString; external name '_NSApplicationWillFinishLaunchingNotification';
  NSApplicationWillResignActiveNotification: NSString; external name '_NSApplicationWillResignActiveNotification';
  NSApplicationWillUnhideNotification: NSString; external name '_NSApplicationWillUnhideNotification';
  NSApplicationWillUpdateNotification: NSString; external name '_NSApplicationWillUpdateNotification';
  NSApplicationWillTerminateNotification: NSString; external name '_NSApplicationWillTerminateNotification';
  NSApplicationDidChangeScreenParametersNotification: NSString; external name '_NSApplicationDidChangeScreenParametersNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSApplication = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLICATION_PAS_C}
{$define NSAPPLICATION_PAS_C}

{ NSApplication }
  NSApplication = objcclass(NSResponder, NSUserInterfaceValidationsProtocol)
  private
    __currentEvent: NSEvent;
    __windowList: id;
    __keyWindow: id;
    __mainWindow: id;
    __delegate: id;
    __hiddenList: id;
    __hiddenCount: cint;
    __context: NSInteger;
    __appleEventSuspensionID: Pointer;
    __previousKeyWindow: id; {garbage collector: __weak }
    __unusedApp: cshort;
    __running: cshort;
    __appFlags: bitpacked record
        _hidden: 0..1;
        _RESERVED1: 0..1;
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
        _skipWin32DelayedRestoreKeyWindowAfterHide: 0..1;
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
        _reserved: 0..((1 shl 6)-1);
      end;
    __mainMenu: id;
    __appIcon: id;
    __nameTable: id;
    __eventDelegate: id;
    __threadingSupport: _NSThreadPrivate;
    
  public
    class function alloc: NSApplication; message 'alloc';

    class function sharedApplication: NSApplication; message 'sharedApplication';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function context: NSGraphicsContext; message 'context';
    procedure hide(sender: id); message 'hide:';
    procedure unhide(sender: id); message 'unhide:';
    procedure unhideWithoutActivation; message 'unhideWithoutActivation';
    function windowWithWindowNumber(windowNum: NSInteger): NSWindow; message 'windowWithWindowNumber:';
    function mainWindow: NSWindow; message 'mainWindow';
    function keyWindow: NSWindow; message 'keyWindow';
    function isActive: Boolean; message 'isActive';
    function isHidden: Boolean; message 'isHidden';
    function isRunning: Boolean; message 'isRunning';
    procedure deactivate; message 'deactivate';
    procedure activateIgnoringOtherApps(flag: Boolean); message 'activateIgnoringOtherApps:';
    procedure hideOtherApplications(sender: id); message 'hideOtherApplications:';
    procedure unhideAllApplications(sender: id); message 'unhideAllApplications:';
    procedure finishLaunching; message 'finishLaunching';
    procedure run; message 'run';
    function runModalForWindow(theWindow: NSWindow): NSInteger; message 'runModalForWindow:';
    procedure stop(sender: id); message 'stop:';
    procedure stopModal; message 'stopModal';
    procedure stopModalWithCode(returnCode: NSInteger); message 'stopModalWithCode:';
    procedure abortModal; message 'abortModal';
    function modalWindow: NSWindow; message 'modalWindow';
    function beginModalSessionForWindow(theWindow: NSWindow): NSModalSession; message 'beginModalSessionForWindow:';
    function runModalSession(session: NSModalSession): NSInteger; message 'runModalSession:';
    procedure endModalSession(session: NSModalSession); message 'endModalSession:';
    procedure terminate(sender: id); message 'terminate:';
    function requestUserAttention(requestType: NSRequestUserAttentionType): NSInteger; message 'requestUserAttention:';
    procedure cancelUserAttentionRequest(request: NSInteger); message 'cancelUserAttentionRequest:';
    procedure beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo(sheet: NSWindow; docWindow: NSWindow; modalDelegate: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
    procedure endSheet(sheet: NSWindow); message 'endSheet:';
    procedure endSheet_returnCode(sheet: NSWindow; returnCode: NSInteger); message 'endSheet:returnCode:';
    function runModalForWindow_relativeToWindow(theWindow: NSWindow; docWindow: NSWindow): NSInteger; message 'runModalForWindow:relativeToWindow:';
    function beginModalSessionForWindow_relativeToWindow(theWindow: NSWindow; docWindow: NSWindow): NSModalSession; message 'beginModalSessionForWindow:relativeToWindow:';
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: Boolean): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure discardEventsMatchingMask_beforeEvent(mask: NSUInteger; lastEvent: NSEvent); message 'discardEventsMatchingMask:beforeEvent:';
    procedure postEvent_atStart(event: NSEvent; flag: Boolean); message 'postEvent:atStart:';
    function currentEvent: NSEvent; message 'currentEvent';
    procedure sendEvent(theEvent: NSEvent); message 'sendEvent:';
    procedure preventWindowOrdering; message 'preventWindowOrdering';
    function makeWindowsPerform_inOrder(aSelector: SEL; flag: Boolean): NSWindow; message 'makeWindowsPerform:inOrder:';
    function windows: NSArray; message 'windows';
    procedure setWindowsNeedUpdate(needUpdate: Boolean); message 'setWindowsNeedUpdate:';
    procedure updateWindows; message 'updateWindows';
    procedure setMainMenu(aMenu: NSMenu); message 'setMainMenu:';
    function mainMenu: NSMenu; message 'mainMenu';
    procedure setApplicationIconImage(image: NSImage); message 'setApplicationIconImage:';
    function applicationIconImage: NSImage; message 'applicationIconImage';
    function dockTile: NSDockTile; message 'dockTile';
    function sendAction_to_from(theAction: SEL; theTarget: id; sender: id): Boolean; message 'sendAction:to:from:';
    function targetForAction(theAction: SEL): id; message 'targetForAction:';
    function targetForAction_to_from(theAction: SEL; theTarget: id; sender: id): id; message 'targetForAction:to:from:';
    function tryToPerform_with(anAction: SEL; anObject: id): Boolean; message 'tryToPerform:with:';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure reportException(theException: NSException); message 'reportException:';
    class procedure detachDrawingThread_toTarget_withObject(selector: SEL; target: id; argument: id); message 'detachDrawingThread:toTarget:withObject:';
    procedure replyToApplicationShouldTerminate(shouldTerminate: Boolean); message 'replyToApplicationShouldTerminate:';
    procedure replyToOpenOrPrint(reply: NSApplicationDelegateReply); message 'replyToOpenOrPrint:';
    procedure orderFrontCharacterPalette(sender: id); message 'orderFrontCharacterPalette:';

    { Category: NSWindowsMenu }
    procedure setWindowsMenu(aMenu: NSMenu); message 'setWindowsMenu:';
    function windowsMenu: NSMenu; message 'windowsMenu';
    procedure arrangeInFront(sender: id); message 'arrangeInFront:';
    procedure removeWindowsItem(win: NSWindow); message 'removeWindowsItem:';
    procedure addWindowsItem_title_filename(win: NSWindow; aString: NSString; isFilename: Boolean); message 'addWindowsItem:title:filename:';
    procedure changeWindowsItem_title_filename(win: NSWindow; aString: NSString; isFilename: Boolean); message 'changeWindowsItem:title:filename:';
    procedure updateWindowsItem(win: NSWindow); message 'updateWindowsItem:';
    procedure miniaturizeAll(sender: id); message 'miniaturizeAll:';

    { Category: NSServicesMenu }
    procedure setServicesMenu(aMenu: NSMenu); message 'setServicesMenu:';
    function servicesMenu: NSMenu; message 'servicesMenu';
    procedure registerServicesMenuSendTypes_returnTypes(sendTypes: NSArray; returnTypes: NSArray); message 'registerServicesMenuSendTypes:returnTypes:';

    { Category: NSServicesHandling }
    procedure setServicesProvider(provider: id); message 'setServicesProvider:';
    function servicesProvider: id; message 'servicesProvider';

    { Category: NSStandardAboutPanel }
    procedure orderFrontStandardAboutPanel(sender: id); message 'orderFrontStandardAboutPanel:';
    procedure orderFrontStandardAboutPanelWithOptions(optionsDictionary: NSDictionary); message 'orderFrontStandardAboutPanelWithOptions:';

    { Category: NSApplicationHelpExtension }
    procedure activateContextHelpMode(sender: id); message 'activateContextHelpMode:';
    procedure showHelp(sender: id); message 'showHelp:';

    { Category: NSPageLayoutPanel }
    procedure runPageLayout(sender: id); message 'runPageLayout:';

    { Category: NSColorPanel }
    procedure orderFrontColorPanel(sender: id); message 'orderFrontColorPanel:';

    { Category: NSScripting }
    function orderedDocuments: NSArray; message 'orderedDocuments';
    function orderedWindows: NSArray; message 'orderedWindows';
  end; external;

{$endif}
{$endif}

{ Parsed from AppKit.framework NSWindowController.h }


{$ifdef TYPES}
type
  NSWindowControllerPtr = ^NSWindowController;
{$endif}

{$ifdef CLASSES}

type
  NSWindowController = objcclass external (NSResponder, NSCodingProtocol, NSSeguePerformingProtocol)
  private
    _window: NSWindow;
    _windowNibName: NSString;
    _document: NSDocument;
    _topLevelObjects: NSArray;
    _owner: id;
    __wcFlags: bitpacked record
      case byte of
        0: (_anonBitField___wcFlags0: cuint);
        1: (
          shouldCloseDocument: 0..1;
          shouldCascade: 0..1;
          nibIsLoaded: 0..1;
          nibNameIsPath: 0..1;
          settingWindowsContentViewController: 0..1;
          didInitWithCoder: 0..1;
          nibIsMakingConnections: 0..1;
          sentWindowWillLoad: 0..1;
          RESERVED: 0..((1 shl 24)-1);
        );
      end;
    _frameAutosaveName: NSString;
    _moreVars: id;
  public
    function initWithWindow (window: NSWindow): instancetype; message 'initWithWindow:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function initWithWindowNibName (windowNibName: NSString): instancetype; message 'initWithWindowNibName:';
    function initWithWindowNibName_owner (windowNibName: NSString; owner: id): instancetype; message 'initWithWindowNibName:owner:';
    function initWithWindowNibPath_owner (windowNibPath: NSString; owner: id): instancetype; message 'initWithWindowNibPath:owner:';
    function windowNibName: NSString; message 'windowNibName';
    function windowNibPath: NSString; message 'windowNibPath';
    function owner: id; message 'owner';
    procedure setWindowFrameAutosaveName(newValue: NSString); message 'setWindowFrameAutosaveName:';
    function windowFrameAutosaveName: NSString; message 'windowFrameAutosaveName';
    procedure setShouldCascadeWindows(newValue: ObjCBOOL); message 'setShouldCascadeWindows:';
    function shouldCascadeWindows: ObjCBOOL; message 'shouldCascadeWindows';
    procedure setDocument(newValue: id); message 'setDocument:';
    function document: id; message 'document';
    procedure setDocumentEdited (dirtyFlag: ObjCBOOL); message 'setDocumentEdited:';
    procedure setShouldCloseDocument(newValue: ObjCBOOL); message 'setShouldCloseDocument:';
    function shouldCloseDocument: ObjCBOOL; message 'shouldCloseDocument';
    procedure synchronizeWindowTitleWithDocumentName; message 'synchronizeWindowTitleWithDocumentName';
    function windowTitleForDocumentDisplayName (displayName: NSString): NSString; message 'windowTitleForDocumentDisplayName:';
    procedure setContentViewController(newValue: NSViewController); message 'setContentViewController:';
    function contentViewController: NSViewController; message 'contentViewController';
    procedure setWindow(newValue: NSWindow); message 'setWindow:';
    function window: NSWindow; message 'window';
    function isWindowLoaded: ObjCBOOL; message 'isWindowLoaded';
    procedure windowWillLoad; message 'windowWillLoad';
    procedure windowDidLoad; message 'windowDidLoad';
    procedure loadWindow; message 'loadWindow';
    procedure close; message 'close';
    procedure showWindow (sender: id); message 'showWindow:';

    { Adopted protocols }
    procedure performSegueWithIdentifier_sender (identifier: NSString; sender: id); message 'performSegueWithIdentifier:sender:'; { available in 10_10 }
    procedure prepareForSegue_sender (segue: NSStoryboardSegue; sender: id); message 'prepareForSegue:sender:'; { available in 10_10 }
    function shouldPerformSegueWithIdentifier_sender (identifier: NSString; sender: id): ObjCBOOL; message 'shouldPerformSegueWithIdentifier:sender:'; { available in 10_10 }
  end;


type
  NSWindowControllerStoryboardingMethods = objccategory external (NSWindowController)
    function storyboard: NSStoryboard; message 'storyboard';
  end;

type
  NSWindowControllerDismissing = objccategory external (NSWindowController)
    procedure dismissController (sender: id); message 'dismissController:'; { available in 10_10 }
  end;
{$endif}


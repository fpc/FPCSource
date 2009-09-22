{ Parsed from Appkit.framework NSWindowController.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSWINDOWCONTROLLER_PAS_H}
{$define NSWINDOWCONTROLLER_PAS_H}
type
  NSWindowControllerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSWINDOWCONTROLLER_PAS_T}
{$define NSWINDOWCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSWINDOWCONTROLLER_PAS_R}
{$define NSWINDOWCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSWINDOWCONTROLLER_PAS_F}
{$define NSWINDOWCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSWINDOWCONTROLLER_PAS_T}
{$define NSWINDOWCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSWindowController = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSWINDOWCONTROLLER_PAS_C}
{$define NSWINDOWCONTROLLER_PAS_C}

{ NSWindowController }
  NSWindowController = objcclass(NSResponder, NSCodingProtocol)
  private
    __window: NSWindow;
    __windowNibName: NSString;
    __document: NSDocument;
    __topLevelObjects: NSArray;
    __owner: id;
    __wcFlags: bitpacked record
        shouldCloseDocument: 0..1;
        shouldCascade: 0..1;
        nibIsLoaded: 0..1;
        nibNameIsPath: 0..1;
        RESERVED: 0..((1 shl 28)-1);
      end;
    __frameAutosaveName: NSString;
    __moreVars: id;
    
  public
    class function alloc: NSWindowController; message 'alloc';

    function initWithWindow(window_: NSWindow): id; message 'initWithWindow:';
    function initWithWindowNibName(windowNibName_: NSString): id; message 'initWithWindowNibName:';
    function initWithWindowNibName_owner(windowNibName_: NSString; owner_: id): id; message 'initWithWindowNibName:owner:';
    function initWithWindowNibPath_owner(windowNibPath_: NSString; owner_: id): id; message 'initWithWindowNibPath:owner:';
    function windowNibName: NSString; message 'windowNibName';
    function windowNibPath: NSString; message 'windowNibPath';
    function owner: id; message 'owner';
    procedure setWindowFrameAutosaveName(name: NSString); message 'setWindowFrameAutosaveName:';
    function windowFrameAutosaveName: NSString; message 'windowFrameAutosaveName';
    procedure setShouldCascadeWindows(flag: Boolean); message 'setShouldCascadeWindows:';
    function shouldCascadeWindows: Boolean; message 'shouldCascadeWindows';
    function document: id; message 'document';
    procedure setDocument(document_: NSDocument); message 'setDocument:';
    procedure setDocumentEdited(dirtyFlag: Boolean); message 'setDocumentEdited:';
    procedure setShouldCloseDocument(flag: Boolean); message 'setShouldCloseDocument:';
    function shouldCloseDocument: Boolean; message 'shouldCloseDocument';
    procedure setWindow(window_: NSWindow); message 'setWindow:';
    function window: NSWindow; message 'window';
    procedure synchronizeWindowTitleWithDocumentName; message 'synchronizeWindowTitleWithDocumentName';
    function windowTitleForDocumentDisplayName(displayName: NSString): NSString; message 'windowTitleForDocumentDisplayName:';
    procedure close; message 'close';
    procedure showWindow(sender: id); message 'showWindow:';
    function isWindowLoaded: Boolean; message 'isWindowLoaded';
    procedure windowWillLoad; message 'windowWillLoad';
    procedure windowDidLoad; message 'windowDidLoad';
    procedure loadWindow; message 'loadWindow';
  end; external;

{$endif}
{$endif}

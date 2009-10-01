{ Parsed from Appkit.framework NSViewController.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSVIEWCONTROLLER_PAS_T}
{$define NSVIEWCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSVIEWCONTROLLER_PAS_R}
{$define NSVIEWCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSVIEWCONTROLLER_PAS_F}
{$define NSVIEWCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSVIEWCONTROLLER_PAS_S}
{$define NSVIEWCONTROLLER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSViewController = objcclass;
  NSViewControllerPointer = ^NSViewController;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVIEWCONTROLLER_PAS_C}
{$define NSVIEWCONTROLLER_PAS_C}

{ NSViewController }
  NSViewController = objcclass(NSResponder)
  private
    __nibName: NSString;
    __nibBundle: NSBundle;
    __representedObject: id;
    __title: NSString;
    _view: NSView;
    __topLevelObjects: NSArray;
    __editors: NSPointerArray;
    __autounbinder: id;
        _reserved: array[0..2] of id;
    
  public
    class function alloc: NSViewController; message 'alloc';

    function initWithNibName_bundle(nibNameOrNil: NSString; nibBundleOrNil: NSBundle): id; message 'initWithNibName:bundle:';
    procedure setRepresentedObject(representedObject_: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    procedure setTitle(title_: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function view: NSView; message 'view';
    procedure loadView; message 'loadView';
    function nibName: NSString; message 'nibName';
    function nibBundle: NSBundle; message 'nibBundle';
    procedure setView(view_: NSView); message 'setView:';
    procedure commitEditingWithDelegate_didCommitSelector_contextInfo(delegate: id; didCommitSelector: SEL; contextInfo: Pointer); message 'commitEditingWithDelegate:didCommitSelector:contextInfo:';
    function commitEditing: Boolean; message 'commitEditing';
    procedure discardEditing; message 'discardEditing';
  end; external;

{$endif}
{$endif}

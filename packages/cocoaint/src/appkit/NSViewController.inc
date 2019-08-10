{ Parsed from AppKit.framework NSViewController.h }


{$ifdef TYPES}
type
  NSViewControllerPtr = ^NSViewController;
  NSViewControllerPresentationAnimatorProtocolPtr = ^NSViewControllerPresentationAnimatorProtocol;
{$endif}

{$ifdef TYPES}
type
  NSViewControllerTransitionOptions = NSUInteger;
  NSViewControllerTransitionOptionsPtr = ^NSViewControllerTransitionOptions;

const
  NSViewControllerTransitionNone = $0;
  NSViewControllerTransitionCrossfade = $1;
  NSViewControllerTransitionSlideUp = $10;
  NSViewControllerTransitionSlideDown = $20;
  NSViewControllerTransitionSlideLeft = $40;
  NSViewControllerTransitionSlideRight = $80;
  NSViewControllerTransitionSlideForward = $140;
  NSViewControllerTransitionSlideBackward = $180;
  NSViewControllerTransitionAllowUserInteraction = $1000;
{$endif}

{$ifdef CLASSES}

type
  NSViewController = objcclass external (NSResponder, NSCodingProtocol, NSSeguePerformingProtocol, NSUserInterfaceItemIdentificationProtocol)
  private
    _nibName: NSString;
    _nibBundle: NSBundle;
    _representedObject: id;
    _title: NSString;
    _view: NSView;
    _topLevelObjects: NSArray;
    _editors: NSPointerArray;
    _autounbinder: id;
    _designNibBundleIdentifier: NSString;
    __privateData: id;
    _anonStruct_NSViewController0: record
      case byte of
        0: (anonBitField_NSViewController0: cuint);
        1: (data: bitpacked record
          _viewIsAppearing: 0..1;
          _delayViewDidAppear: 0..1;
          _isContentViewController: 0..1;
          _reserved: 0..((1 shl 29)-1);
        end;
      );
          end;
  public
    function initWithNibName_bundle (nibNameOrNil: NSString; nibBundleOrNil: NSBundle): instancetype; message 'initWithNibName:bundle:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function nibName: NSString; message 'nibName';
    function nibBundle: NSBundle; message 'nibBundle';
    procedure setRepresentedObject(newValue: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure loadView; message 'loadView';
    procedure commitEditingWithDelegate_didCommitSelector_contextInfo (delegate: id; didCommitSelector: SEL; contextInfo: pointer); message 'commitEditingWithDelegate:didCommitSelector:contextInfo:';
    function commitEditing: ObjCBOOL; message 'commitEditing';
    procedure discardEditing; message 'discardEditing';
    procedure viewDidLoad; message 'viewDidLoad'; { available in 10_10 }
    function isViewLoaded: ObjCBOOL; message 'isViewLoaded';
    procedure viewWillAppear; message 'viewWillAppear'; { available in 10_10 }
    procedure viewDidAppear; message 'viewDidAppear'; { available in 10_10 }
    procedure viewWillDisappear; message 'viewWillDisappear'; { available in 10_10 }
    procedure viewDidDisappear; message 'viewDidDisappear'; { available in 10_10 }
    procedure setPreferredContentSize(newValue: NSSize); message 'setPreferredContentSize:';
    function preferredContentSize: NSSize; message 'preferredContentSize';
    procedure updateViewConstraints; message 'updateViewConstraints'; { available in 10_10 }
    procedure viewWillLayout; message 'viewWillLayout'; { available in 10_10 }
    procedure viewDidLayout; message 'viewDidLayout'; { available in 10_10 }

    { Adopted protocols }
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    procedure performSegueWithIdentifier_sender (identifier: NSString; sender: id); message 'performSegueWithIdentifier:sender:'; { available in 10_10 }
    procedure prepareForSegue_sender (segue: NSStoryboardSegue; sender: id); message 'prepareForSegue:sender:'; { available in 10_10 }
    function shouldPerformSegueWithIdentifier_sender (identifier: NSString; sender: id): ObjCBOOL; message 'shouldPerformSegueWithIdentifier:sender:'; { available in 10_10 }
  end;


type
  NSViewControllerPresentation = objccategory external (NSViewController)
    procedure presentViewController_animator (viewController: NSViewController; animator: NSViewControllerPresentationAnimatorProtocol); message 'presentViewController:animator:'; { available in 10_10 }
    procedure dismissViewController (viewController: NSViewController); message 'dismissViewController:'; { available in 10_10 }
    procedure dismissController (sender: id); message 'dismissController:'; { available in 10_10 }
    function presentedViewControllers: NSArray; message 'presentedViewControllers';
    function presentingViewController: NSViewController; message 'presentingViewController';
  end;

type
  NSViewControllerPresentationAndTransitionStyles = objccategory external (NSViewController)
    procedure presentViewControllerAsSheet (viewController: NSViewController); message 'presentViewControllerAsSheet:'; { available in 10_10 }
    procedure presentViewControllerAsModalWindow (viewController: NSViewController); message 'presentViewControllerAsModalWindow:'; { available in 10_10 }
    procedure presentViewController_asPopoverRelativeToRect_ofView_preferredEdge_behavior (viewController: NSViewController; positioningRect: NSRect; positioningView: NSView; preferredEdge: NSRectEdge; behavior: NSPopoverBehavior); message 'presentViewController:asPopoverRelativeToRect:ofView:preferredEdge:behavior:'; { available in 10_10 }
    procedure transitionFromViewController_toViewController_options_completionHandler (fromViewController: NSViewController; toViewController: NSViewController; options: NSViewControllerTransitionOptions; completion: OpaqueCBlock); message 'transitionFromViewController:toViewController:options:completionHandler:'; { available in 10_10 }
  end;

type
  NSViewControllerContainer = objccategory external (NSViewController)
    function parentViewController: NSViewController; message 'parentViewController';
    procedure setChildViewControllers(newValue: NSArray); message 'setChildViewControllers:';
    function childViewControllers: NSArray; message 'childViewControllers';
    procedure addChildViewController (childViewController: NSViewController); message 'addChildViewController:'; { available in 10_10 }
    procedure removeFromParentViewController; message 'removeFromParentViewController'; { available in 10_10 }
    procedure insertChildViewController_atIndex (childViewController: NSViewController; index: NSInteger); message 'insertChildViewController:atIndex:'; { available in 10_10 }
    procedure removeChildViewControllerAtIndex (index: NSInteger); message 'removeChildViewControllerAtIndex:'; { available in 10_10 }
    procedure preferredContentSizeDidChangeForViewController (viewController: NSViewController); message 'preferredContentSizeDidChangeForViewController:'; { available in 10_10 }
    procedure viewWillTransitionToSize (newSize: NSSize); message 'viewWillTransitionToSize:'; { available in 10_10 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSViewControllerPresentationAnimatorProtocol = objcprotocol external name 'NSViewControllerPresentationAnimator' (NSObjectProtocol)
  required
    procedure animatePresentationOfViewController_fromViewController (viewController: NSViewController; fromViewController: NSViewController); message 'animatePresentationOfViewController:fromViewController:'; { available in 10_10 }
    procedure animateDismissalOfViewController_fromViewController (viewController: NSViewController; fromViewController: NSViewController); message 'animateDismissalOfViewController:fromViewController:'; { available in 10_10 }
  end;
{$endif}

{$ifdef CLASSES}

type
  NSViewControllerStoryboardingMethods = objccategory external (NSViewController)
    function storyboard: NSStoryboard; message 'storyboard';
  end;
{$endif}

{$if defined(__OBJC2__)}
{$ifdef CLASSES}

type
  NSExtensionAdditions = objccategory external (NSViewController)
    function extensionContext: NSExtensionContext; message 'extensionContext';
    procedure setSourceItemView(newValue: NSView); message 'setSourceItemView:';
    function sourceItemView: NSView; message 'sourceItemView';
    procedure setPreferredScreenOrigin(newValue: NSPoint); message 'setPreferredScreenOrigin:';
    function preferredScreenOrigin: NSPoint; message 'preferredScreenOrigin';
    function preferredMinimumSize: NSSize; message 'preferredMinimumSize';
    function preferredMaximumSize: NSSize; message 'preferredMaximumSize';
  end;
{$endif}

{$endif}
